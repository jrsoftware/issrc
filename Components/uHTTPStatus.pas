(*
 * DelphiEasyRest
 * Copyright (c) 2019 Gleison Paulo Caldeira Oliveira
 * This file is a part of DelphiEasyRest.
 * https://github.com/gleisonpauloc/DelphiEasyRest
 * This project is licensed under the terms of the MIT license.
*)
//HTTP status code from: https://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml

unit uHTTPStatus;

interface

const
    StatusContinue                            = 100;         //[RFC7231, Section 6.2.1]
    StatusSwitchingProtocols                  = 101;         //[RFC7231, Section 6.2.2]
    StatusProcessing                          = 102;         //[RFC2518]
    StatusEarlyHints                          = 103;         //[RFC8297]

    StatusOK                                  = 200;         //[RFC7231, Section 6.3.1]
    StatusCreated                             = 201;         //[RFC7231, Section 6.3.2]
    StatusAccepted                            = 202;         //[RFC7231, Section 6.3.3]
    StatusNonAuthoritativeInformation         = 203;         //[RFC7231, Section 6.3.4]
    StatusNoContent                           = 204;         //[RFC7231, Section 6.3.5]
    StatusResetContent                        = 205;         //[RFC7231, Section 6.3.6]
    StatusPartialContent                      = 206;         //[RFC7233, Section 4.1]
    StatusMultiStatus                         = 207;         //[RFC4918]
    StatusAlreadyReported                     = 208;         //[RFC5842]

    StatusIMUsed                              = 226;         //[RFC3229]

    StatusMultipleChoices                     = 300;         //[RFC7231, Section 6.4.1]
    StatusMovedPermanently                    = 301;         //[RFC7231, Section 6.4.2]
    StatusFound                               = 302;         //[RFC7231, Section 6.4.3]
    StatusSeeOther                            = 303;         //[RFC7231, Section 6.4.4]
    StatusNotModified                         = 304;         //[RFC7232, Section 4.1]
    StatusUseProxy                            = 305;         //[RFC7231, Section 6.4.5]
    StatusUnused                              = 306;         //[RFC7231, Section 6.4.6]
    StatusTemporaryRedirect                   = 307;         //[RFC7231, Section 6.4.7]
    StatusPermanentRedirect                   = 308;         //[RFC7538]

    StatusBadRequest                          = 400;         //[RFC7231, Section 6.5.1]
    StatusUnauthorized                        = 401;         //[RFC7235, Section 3.1]
    StatusPaymentRequired                     = 402;         //[RFC7231, Section 6.5.2]
    StatusForbidden                           = 403;         //[RFC7231, Section 6.5.3]
    StatusNotFound                            = 404;         //[RFC7231, Section 6.5.4]
    StatusMethodNotAllowed                    = 405;         //[RFC7231, Section 6.5.5]
    StatusNotAcceptable                       = 406;         //[RFC7231, Section 6.5.6]
    StatusProxyAuthenticationRequired         = 407;         //[RFC7235, Section 3.2]
    StatusRequestTimeout                      = 408;         //[RFC7231, Section 6.5.7]
    StatusConflict                            = 409;         //[RFC7231, Section 6.5.8]
    StatusGone                                = 410;         //[RFC7231, Section 6.5.9]
    StatusLengthRequired                      = 411;         //[RFC7231, Section 6.5.10]
    StatusPreconditionFailed                  = 412;         //[RFC7232, Section 4.2]                //[RFC8144, Section 3.2]
    StatusPayloadTooLarge                     = 413;         //[RFC7231, Section 6.5.11]
    StatusURITooLong                          = 414;         //[RFC7231, Section 6.5.12]
    StatusUnsupportedMediaType                = 415;         //[RFC7231, Section 6.5.13]             //[RFC7694, Section 3]
    StatusRangeNotSatisfiable                 = 416;         //[RFC7233, Section 4.4]
    StatusExpectationFailed                   = 417;         //[RFC7231, Section 6.5.14]

    StatusMisdirectedRequest                  = 421;         //[RFC7540, Section 9.1.2]
    StatusUnprocessableEntity                 = 422;         //[RFC4918]
    StatusLocked                              = 423;         //[RFC4918]
    StatusFailedDependency                    = 424;         //[RFC4918]
    StatusTooEarly                            = 425;         //[RFC8470]
    StatusUpgradeRequired                     = 426;         //[RFC7231, Section 6.5.15]

    StatusPreconditionRequired                = 428;         //[RFC6585]
    StatusTooManyRequests                     = 429;         //[RFC6585]

    StatusRequestHeaderFieldsTooLarge         = 431;         //[RFC6585]

    StatusUnavailableForLegalReasons          = 451;         //[RFC7725]

    StatusInternalServerError                 = 500;         //[RFC7231, Section 6.6.1]
    StatusNotImplemented                      = 501;         //[RFC7231, Section 6.6.2]
    StatusBadGateway                          = 502;         //[RFC7231, Section 6.6.3]
    StatusServiceUnavailable                  = 503;         //[RFC7231, Section 6.6.4]
    StatusGatewayTimeout                      = 504;         //[RFC7231, Section 6.6.5]
    StatusHTTPVersionNotSupported             = 505;         //[RFC7231, Section 6.6.6]
    StatusVariantAlsoNegotiates               = 506;         //[RFC2295]
    StatusInsufficientStorage                 = 507;         //[RFC4918]
    StatusLoopDetected                        = 508;         //[RFC5842]

    StatusNotExtended                         = 510;         //[RFC2774]
    StatusNetworkAuthenticationRequired       = 511;         //[RFC6585]


function StatusText(const AStatusCode: Integer): string;

implementation

uses
  System.SysUtils;

function StatusText(const AStatusCode: Integer): string;
begin
  case AStatusCode of
     StatusContinue                       : Result := 'Continue';
     StatusSwitchingProtocols             : Result := 'Switching Protocols';
     StatusProcessing                     : Result := 'Processing';
     StatusEarlyHints                     : Result := 'Early Hints';
     StatusOK                             : Result := 'OK';
     StatusCreated                        : Result := 'Created';
     StatusAccepted                       : Result := 'Accepted';
     StatusNonAuthoritativeInformation    : Result := 'Non-Authoritative Information';
     StatusNoContent                      : Result := 'No Content';
     StatusResetContent                   : Result := 'Reset Content';
     StatusPartialContent                 : Result := 'Partial Content';
     StatusMultiStatus                    : Result := 'Multi-Status';
     StatusAlreadyReported                : Result := 'Already Reported';
     StatusIMUsed                         : Result := 'IM Used';
     StatusMultipleChoices                : Result := 'Multiple Choices';
     StatusMovedPermanently               : Result := 'Moved Permanently';
     StatusFound                          : Result := 'Found';
     StatusSeeOther                       : Result := 'See Other';
     StatusNotModified                    : Result := 'Not Modified';
     StatusUseProxy                       : Result := 'Use Proxy';
     StatusUnused                         : Result := '(Unused)';
     StatusTemporaryRedirect              : Result := 'Temporary Redirect';
     StatusPermanentRedirect              : Result := 'Permanent Redirect';
     StatusBadRequest                     : Result := 'Bad Request';
     StatusUnauthorized                   : Result := 'Unauthorized';
     StatusPaymentRequired                : Result := 'Payment Required';
     StatusForbidden                      : Result := 'Forbidden';
     StatusNotFound                       : Result := 'Not Found';
     StatusMethodNotAllowed               : Result := 'Method Not Allowed';
     StatusNotAcceptable                  : Result := 'Not Acceptable';
     StatusProxyAuthenticationRequired    : Result := 'Proxy Authentication Required';
     StatusRequestTimeout                 : Result := 'Request Timeout';
     StatusConflict                       : Result := 'Conflict';
     StatusGone                           : Result := 'Gone';
     StatusLengthRequired                 : Result := 'Length Required';
     StatusPreconditionFailed             : Result := 'Precondition Failed';
     StatusPayloadTooLarge                : Result := 'Payload Too Large';
     StatusURITooLong                     : Result := 'URI Too Long';
     StatusUnsupportedMediaType           : Result := 'Unsupported Media Type';
     StatusRangeNotSatisfiable            : Result := 'Range Not Satisfiable';
     StatusExpectationFailed              : Result := 'Expectation Failed';
     StatusMisdirectedRequest             : Result := 'Misdirected Request';
     StatusUnprocessableEntity            : Result := 'Unprocessable Entity';
     StatusLocked                         : Result := 'Locked';
     StatusFailedDependency               : Result := 'Failed Dependency';
     StatusTooEarly                       : Result := 'Too Early';
     StatusUpgradeRequired                : Result := 'Upgrade Required';
     StatusPreconditionRequired           : Result := 'Precondition Required';
     StatusTooManyRequests                : Result := 'Too Many Requests';
     StatusRequestHeaderFieldsTooLarge    : Result := 'Request Header Fields Too Large';
     StatusUnavailableForLegalReasons     : Result := 'Unavailable For Legal Reasons';
     StatusInternalServerError            : Result := 'Internal Server Error';
     StatusNotImplemented                 : Result := 'Not Implemented';
     StatusBadGateway                     : Result := 'Bad Gateway';
     StatusServiceUnavailable             : Result := 'Service Unavailable';
     StatusGatewayTimeout                 : Result := 'Gateway Timeout';
     StatusHTTPVersionNotSupported        : Result := 'HTTP Version Not Supported';
     StatusVariantAlsoNegotiates          : Result := 'Variant Also Negotiates';
     StatusInsufficientStorage            : Result := 'Insufficient Storage';
     StatusLoopDetected                   : Result := 'Loop Detected';
     StatusNotExtended                    : Result := 'Not Extended';
     StatusNetworkAuthenticationRequired  : Result := 'Network Authentication Required';
  else
    raise Exception.CreateFmt('Unknow status code: %d', [AStatusCode]);
  end;
end;


end.
