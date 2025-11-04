#' @include provider.R
#' @include content.R
#' @include turns.R
#' @include tools-def.R
NULL

# ============================================================================
# Public constructor
# ============================================================================

#' Chat with Amazon Q Business (QBusiness Chat API)
#'
#' Uses AWS SigV4 auth via \{paws.common\}. If you use AWS SSO, run `aws sso login`.
#'
#' @param application_id Q Business applicationId (required, 36-char ID).
#' @param system_prompt Optional system prompt. Q Chat has no native "system";
#'   we inline this into the first user message for now.
#' @param base_url Optional base URL. Default: https://qbusiness.\{region\}.amazonaws.com
#' @param profile AWS profile name (optional).
#' @param user_id Optional Q userId associated to the chat input.
#' @param user_groups Optional character vector of user groups (comma-joined into query).
#' @param client_token Optional idempotency token. If NULL, a random hex token is generated.
#' @param conversation_id Optional conversationId to continue a thread.
#' @param parent_message_id Optional parent message ID (to thread replies).
#' @param params Common model params; usually ignored by Q Chat (kept for API symmetry).
#' @param api_args Named list merged into request body (e.g. configurationEvent for streaming,
#'   or chatMode/chatModeConfiguration/attributeFilter for sync).
#' @param api_headers Named character vector of extra headers.
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @family chatbots
#' @export
chat_aws_q <- function(
  application_id,
  system_prompt = NULL,
  base_url = NULL,
  profile = NULL,
  user_id = NULL,
  user_groups = NULL,
  client_token = NULL,
  conversation_id = NULL,
  parent_message_id = NULL,
  params = NULL,
  api_args = list(),
  api_headers = character(),
  echo = NULL
) {
  check_installed("paws.common", "AWS authentication")
  check_string(application_id)
  check_string(base_url, allow_null = TRUE)
  echo <- check_echo(echo)

  params <- params %||% params()

  provider <- provider_aws_qbusiness(
    base_url = base_url,
    profile = profile,
    application_id = application_id,
    user_id = user_id,
    user_groups = user_groups,
    client_token = client_token,
    conversation_id = conversation_id,
    parent_message_id = parent_message_id,
    params = params,
    extra_args = api_args,
    extra_headers = api_headers
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

# ============================================================================
# Provider constructor/class
# ============================================================================

provider_aws_qbusiness <- function(
  base_url,
  profile = NULL,
  application_id,
  user_id = NULL,
  user_groups = NULL,
  client_token = NULL,
  conversation_id = NULL,
  parent_message_id = NULL,
  params = list(),
  extra_args = list(),
  extra_headers = character()
) {
  cache <- aws_creds_cache(profile)
  credentials <- paws_credentials(profile, cache = cache)

  if (is.function(base_url)) {
    base_url <- base_url(credentials$region)
  } else {
    base_url <- base_url %||%
      sprintf("https://qbusiness.%s.amazonaws.com", credentials$region)
  }

  generate_client_token <- function() {
    paste(
      sprintf("%02x", sample.int(256, 16, replace = TRUE) - 1L),
      collapse = ""
    )
  }
  client_token <- client_token %||% generate_client_token()

  user_groups_str <- if (is.null(user_groups)) {
    NULL
  } else {
    paste0(as.character(user_groups), collapse = ",")
  }

  # Provider requires 'model'; Q doesn't use it, so set "".
  ProviderAWSQBusiness(
    name = "AWS/QBusiness",
    model = "",
    base_url = base_url,
    params = params,
    extra_args = extra_args,
    extra_headers = extra_headers,
    credentials = NULL,
    # subclass props
    profile = profile,
    region = credentials$region,
    application_id = application_id,
    user_id = user_id,
    user_groups = user_groups_str,
    client_token = client_token,
    conversation_id = conversation_id,
    parent_message_id = parent_message_id,
    cache = cache
  )
}

ProviderAWSQBusiness <- new_class(
  "ProviderAWSQBusiness",
  parent = Provider,
  properties = list(
    profile = prop_string(allow_null = TRUE),
    region = prop_string(),
    application_id = prop_string(),
    user_id = prop_string(allow_null = TRUE),
    user_groups = prop_string(allow_null = TRUE), # comma-separated
    client_token = prop_string(),
    conversation_id = prop_string(allow_null = TRUE),
    parent_message_id = prop_string(allow_null = TRUE),
    cache = class_list
  )
)

# ============================================================================
# Base request & error handling
# ============================================================================

method(base_request, ProviderAWSQBusiness) <- function(provider) {
  creds <- paws_credentials(provider@profile, provider@cache)

  req <- request(provider@base_url)
  req <- req_auth_aws_v4(
    req,
    aws_access_key_id = creds$access_key_id,
    aws_secret_access_key = creds$secret_access_key,
    aws_session_token = creds$session_token,
    service = "qbusiness",
    region = provider@region
  )
  req <- ellmer_req_robustify(req)
  req <- ellmer_req_user_agent(req)
  req <- base_request_error(provider, req)
  req
}

method(base_request_error, ProviderAWSQBusiness) <- function(provider, req) {
  req_error(req, body = function(resp) {
    b <- tryCatch(resp_body_json(resp), error = function(e) list())
    b$message %||%
      b$errorMessage %||%
      b$Message %||%
      http_status_desc(status_code(resp))
  })
}

# Keep a param mapper for symmetry; typically Q Chat ignores these unless you pass
# them via configurationEvent (stream) or chatMode/chatModeConfiguration (sync).
method(chat_params, ProviderAWSQBusiness) <- function(provider, params) {
  standardise_params(
    params,
    c(
      temperature = "temperature",
      topP = "top_p",
      maxTokens = "max_tokens",
      stopSequences = "stop_sequences"
    )
  )
}

# ============================================================================
# Build the chat request (supports streaming & non-streaming/sync)
# ============================================================================

method(chat_request, ProviderAWSQBusiness) <- function(
  provider,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL
) {
  # Endpoint: POST /applications/{applicationId}/conversations[?sync]
  req <- base_request(provider)
  req <- req_url_path_append(
    req,
    "applications",
    provider@application_id,
    "conversations"
  )

  q <- list(
    clientToken = provider@client_token,
    conversationId = provider@conversation_id,
    parentMessageId = provider@parent_message_id,
    userId = provider@user_id
  )
  if (!is.null(provider@user_groups)) {
    q$userGroups <- provider@user_groups
  }

  if (!stream) {
    # Non-streaming uses ChatSync with ?sync
    # Ref: ChatSync Request Syntax uses '?sync' plus userGroups & userId in query
    # (body uses 'userMessage', 'attachments', and top-level config fields)
    # :contentReference[oaicite:0]{index=0}
    q$sync <- TRUE
  }
  req <- req_url_query(req, !!!compact(q))

  # ----- Gather message text & optional system prompt -----
  msgs <- compact(as_json(provider, turns))

  if (length(turns) >= 1 && is_system_turn(turns[[1]])) {
    sys <- turns[[1]]@text
    if (length(msgs) >= 2 && identical(msgs[[2]]$role, "user")) {
      if (
        length(msgs[[2]]$content) > 0 && !is.null(msgs[[2]]$content[[1]]$text)
      ) {
        msgs[[2]]$content[[1]]$text <- paste0(
          sys,
          "\n\n",
          msgs[[2]]$content[[1]]$text
        )
      }
    }
    keep <- !vapply(turns, is_system_turn, logical(1))
    msgs <- msgs[keep]
  }

  # Last user message is the input text for this call
  last_user_text <- NULL
  last_user_idx <- NA_integer_
  if (length(msgs)) {
    for (i in rev(seq_along(msgs))) {
      if (identical(msgs[[i]]$role, "user")) {
        txts <- vapply(
          msgs[[i]]$content,
          function(z) z$text %||% "",
          FUN.VALUE = character(1)
        )
        last_user_text <- paste0(txts, collapse = "")
        last_user_idx <- i
        break
      }
    }
  }

  # ----- Collect uploadable attachments (PDF / inline images) from that last user turn -----
  atts <- collect_q_attachments_from_turn(msgs, last_user_idx)

  # ----- Build body for streaming vs sync -----
  if (isTRUE(stream)) {
    # Streaming Chat: body is a sequence of *events*
    # textEvent + (optional) attachmentEvent + endOfInputEvent (+ configurationEvent via extra_args)
    body <- list()
    if (!is.null(last_user_text) && nzchar(last_user_text)) {
      body$textEvent <- list(userMessage = last_user_text)
    } else if (length(turns) >= 1 && is_system_turn(turns[[1]])) {
      body$textEvent <- list(userMessage = turns[[1]]@text)
    }

    # Single attachmentEvent supported per request (we take the first if present)
    if (length(atts)) {
      body$attachmentEvent <- list(
        attachment = list(
          name = atts[[1]]$name,
          data = atts[[1]]$data
        )
      )
    }

    # Merge user-supplied events (e.g., configurationEvent)
    body <- modify_list(body, provider@extra_args)

    # Required end-of-input marker
    body$endOfInputEvent <- list()

    req <- req_body_json(req, compact(body))
    req <- req_headers(req, !!!provider@extra_headers)
    req
  } else {
    # Non-streaming ChatSync: body uses different field names
    # - 'userMessage' (string)
    # - 'attachments' (array)
    # - top-level config fields (chatMode, chatModeConfiguration, attributeFilter, etc.)
    # :contentReference[oaicite:1]{index=1}

    sync_body <- list()
    if (!is.null(last_user_text) && nzchar(last_user_text)) {
      sync_body$userMessage <- last_user_text
    } else if (length(turns) >= 1 && is_system_turn(turns[[1]])) {
      sync_body$userMessage <- turns[[1]]@text
    }

    if (length(atts)) {
      sync_body$attachments <- lapply(atts, function(a) {
        list(name = a$name, data = a$data)
      })
    }

    # Map select provider@extra_args into sync fields when users supplied streaming-style names
    # Allow both styles:
    # - If user provided chatMode/chatModeConfiguration/attributeFilter directly -> keep as-is
    # - If user provided configurationEvent=list(chatMode=..., chatModeConfiguration=...) -> map them
    ea <- provider@extra_args
    if (!is.null(ea$configurationEvent)) {
      ce <- ea$configurationEvent
      ea$chatMode <- ea$chatMode %||% ce$chatMode
      ea$chatModeConfiguration <- ea$chatModeConfiguration %||%
        ce$chatModeConfiguration
      ea$attributeFilter <- ea$attributeFilter %||% ce$attributeFilter
      ea$configurationEvent <- NULL
    }
    sync_body <- modify_list(sync_body, ea)

    req <- req_body_json(req, compact(sync_body))
    req <- req_headers(req, !!!provider@extra_headers)
    req
  }
}

# ============================================================================
# Streaming helpers (Q uses AWS event streams; reuse Bedrock reader)
# ============================================================================

method(chat_resp_stream, ProviderAWSQBusiness) <- function(provider, resp) {
  resp_stream_aws(resp)
}

method(stream_parse, ProviderAWSQBusiness) <- function(provider, event) {
  if (is.null(event)) {
    return()
  }
  body <- event$body
  evt <- event$headers$`:event-type` %||%
    event$headers$`x-amz-event-type` %||%
    NULL
  if (!is.null(evt)) {
    body$event_type <- evt
  }
  body
}

method(stream_text, ProviderAWSQBusiness) <- function(provider, event) {
  # Streaming text arrives in textEvent.systemMessage; final text may be in metadataEvent.finalTextMessage
  if (!is.null(event$textEvent) && !is.null(event$textEvent$systemMessage)) {
    return(event$textEvent$systemMessage)
  }
  if (
    !is.null(event$metadataEvent) &&
      !is.null(event$metadataEvent$finalTextMessage)
  ) {
    return(event$metadataEvent$finalTextMessage)
  }
  NULL
}

method(stream_merge_chunks, ProviderAWSQBusiness) <- function(
  provider,
  result,
  chunk
) {
  if (is.null(result)) {
    result <- list(role = "assistant", content = list(list(text = "")))
  }

  # Append streaming text
  if (!is.null(chunk$textEvent) && !is.null(chunk$textEvent$systemMessage)) {
    paste(result$content[[1]]$text) <- chunk$textEvent$systemMessage
  }

  # Capture final text + metadata (citations, IDs, etc.)
  if (!is.null(chunk$metadataEvent)) {
    if (
      !is.null(chunk$metadataEvent$finalTextMessage) &&
        nzchar(chunk$metadataEvent$finalTextMessage)
    ) {
      result$content[[1]]$text <- chunk$metadataEvent$finalTextMessage
    }
    result$q_meta <- chunk$metadataEvent

    # Auto-persist conversationId if present
    if (
      !is.null(chunk$metadataEvent$conversationId) &&
        is.character(chunk$metadataEvent$conversationId) &&
        nzchar(chunk$metadataEvent$conversationId)
    ) {
      provider@conversation_id <- chunk$metadataEvent$conversationId
    }
  }

  # Optional plugin/action review, failed attachments
  if (!is.null(chunk$actionReviewEvent)) {
    result$q_action_review <- chunk$actionReviewEvent
  }
  if (!is.null(chunk$failedAttachmentEvent)) {
    result$q_failed_attachment <- chunk$failedAttachmentEvent
  }

  result
}

# ============================================================================
# Non-streaming response (ChatSync) + tokens
# ============================================================================

method(value_tokens, ProviderAWSQBusiness) <- function(provider, json) {
  # Q Chat API doesn't advertise token usage; return NA to keep API stable.
  tokens(input = NA_integer_, output = NA_integer_)
}

method(value_turn, ProviderAWSQBusiness) <- function(
  provider,
  result,
  has_type = FALSE
) {
  # 'result' is either:
  # - the merged object from stream_merge_chunks() for streaming, or
  # - the ChatSync JSON response for non-streaming:
  #     contains 'systemMessage' plus IDs and possibly attributions
  #     (per API docs for ChatSync response fields)
  # We normalize both to an AssistantTurn(ContentText).

  # Auto-persist conversationId from sync responses if present
  if (is.null(result$q_meta) && !is.null(result$conversationId)) {
    provider@conversation_id <- result$conversationId
  }

  text <- ""
  if (
    !is.null(result$content) &&
      length(result$content) > 0 &&
      !is.null(result$content[[1]]$text)
  ) {
    # streaming aggregate
    text <- result$content[[1]]$text
  } else if (!is.null(result$systemMessage)) {
    # sync response
    text <- result$systemMessage
  }

  contents <- list(ContentText(text))
  tks <- value_tokens(provider, result)
  cost <- get_token_cost(provider, tks)
  AssistantTurn(contents, json = result, tokens = unlist(tks), cost = cost)
}

# ============================================================================
# ellmer -> Q local JSON shims (text + light attachments)
# ============================================================================

method(as_json, list(ProviderAWSQBusiness, Turn)) <- function(
  provider,
  x,
  ...
) {
  if (is_system_turn(x)) {
    list(role = "system", content = as_json(provider, x@contents, ...))
  } else if (is_user_turn(x) || is_assistant_turn(x)) {
    list(role = x@role, content = as_json(provider, x@contents, ...))
  } else {
    cli::cli_abort("Unknown role {x@role}", .internal = TRUE)
  }
}

method(as_json, list(ProviderAWSQBusiness, ContentText)) <- function(
  provider,
  x,
  ...
) {
  if (is_whitespace(x@text)) {
    list(list(text = "[empty string]"))
  } else {
    list(list(text = x@text))
  }
}

# Accept inline image/PDF as potential attachments (we don't send them here; we collect in chat_request)
method(as_json, list(ProviderAWSQBusiness, ContentImageInline)) <- function(
  provider,
  x,
  ...
) {
  # Represent as a placeholder block so we can locate it later in chat_request
  list(list(
    .ellmer_q_attachment = list(
      kind = "image",
      name = x@name %||% "image",
      data = x@data
    )
  ))
}

method(as_json, list(ProviderAWSQBusiness, ContentPDF)) <- function(
  provider,
  x,
  ...
) {
  list(list(
    .ellmer_q_attachment = list(
      kind = "pdf",
      name = x@name %||% "document.pdf",
      data = x@data
    )
  ))
}

method(as_json, list(ProviderAWSQBusiness, ContentImageRemote)) <- function(
  provider,
  x,
  ...
) {
  cli::cli_abort(
    "Q Business Chat adapter: remote images aren't supported; use inline bytes or upload."
  )
}

method(as_json, list(ProviderAWSQBusiness, ContentToolRequest)) <- function(
  provider,
  x,
  ...
) {
  cli::cli_abort(
    "Q Business plugins/actionExecutionEvent not auto-mapped from ToolRequest."
  )
}

method(as_json, list(ProviderAWSQBusiness, ContentToolResult)) <- function(
  provider,
  x,
  ...
) {
  cli::cli_abort(
    "Q Business plugins/action responses not auto-mapped from ToolResult."
  )
}

# ============================================================================
# Helpers
# ============================================================================

# Extract inline attachments from the last user turn
collect_q_attachments_from_turn <- function(msgs, user_idx) {
  if (is.na(user_idx) || user_idx < 1 || user_idx > length(msgs)) {
    return(list())
  }
  cont <- msgs[[user_idx]]$content
  out <- list()
  for (c in cont) {
    if (!is.null(c$.ellmer_q_attachment)) {
      a <- c$.ellmer_q_attachment
      # jsonlite will base64-encode raw vectors for us
      out[[length(out) + 1]] <- list(name = a$name, data = a$data)
    }
  }
  out
}

# Utility: pull Q citations from a turn produced by chat_aws_q()
# Returns a data.frame with citationNumber, title, url, snippet (if present).
#' @keywords internal
#' @noRd
extract_q_citations <- function(turn) {
  meta <- NULL
  if (!is.null(turn@json$q_meta)) {
    meta <- turn@json$q_meta
  } else if (!is.null(turn@json$metadataEvent)) {
    meta <- turn@json$metadataEvent
  }
  if (is.null(meta) || is.null(meta$sourceAttributions)) {
    return(data.frame(
      citationNumber = integer(),
      title = character(),
      url = character(),
      snippet = character()
    ))
  }
  sats <- meta$sourceAttributions
  data.frame(
    citationNumber = vapply(
      sats,
      function(x) x$citationNumber %||% NA_integer_,
      integer(1)
    ),
    title = vapply(sats, function(x) x$title %||% "", character(1)),
    url = vapply(sats, function(x) x$url %||% "", character(1)),
    snippet = vapply(sats, function(x) x$snippet %||% "", character(1)),
    stringsAsFactors = FALSE
  )
}

# ============================================================================
# Credential helpers reused from Bedrock provider:
# - paws_credentials()
# - locate_aws_credentials()
# - aws_creds_cache()
# ============================================================================
