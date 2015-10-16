%
-ifdef(debug).
-define(LOG_INFO(String,Term),
    error_logger:info_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String},
       {term, Term}])).

-define(LOG_INFO(String),
    error_logger:info_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String}])).

-define(LOG_WARNING(String,Term),
    error_logger:warning_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String},
       {term, Term}])).


-define(LOG_WARNING(String),
    error_logger:warning_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String}])).
-else.
-define(LOG_WARNING(String), ok).
-define(LOG_WARNING(String,Term), ok).
-define(LOG_INFO(String), ok).
-define(LOG_INFO(String,Term), ok).
-endif.



-define(LOG_ERROR(String,Term),
    error_logger:error_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String},
       {term, Term}])).

-define(LOG_ERROR(String),
    error_logger:error_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String}])).
