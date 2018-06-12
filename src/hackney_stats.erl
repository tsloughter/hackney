%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
-module(hackney_stats).

-include_lib("opencensus/include/opencensus.hrl").

-export([default_views/0,
         register_measures/0]).

register_measures() ->
    oc_stat_measure:new('opencensus.io/http/client/received_bytes',
                        "Total bytes received in response body (not including headers). "
                        "This is uncompressed bytes.",
                        bytes),
    oc_stat_measure:new('opencensus.io/http/client/sent_bytes',
                        "Total bytes sent in request bodies.",
                        bytes),
    oc_stat_measure:new('opencensus.io/http/client/roundtrip_latency', "Time between first byte of "
                        "request headers read to last byte of response sent, or terminal error.",
                        milli_seconds).


default_views() ->
    [#{name => "opencensus.io/http/client/completed_count",
       description => "Count of HTTP requests completed",
       tags => [http_client_method, http_client_path, http_client_status],
       measure => 'opencensus.io/http/client/roundtrip_latency',
       aggregation => oc_stat_aggregation_count},
    #{name => "opencensus.io/http/client/received_bytes",
       description => "Size distribution of HTTP bytes received",
       tags => [http_client_method, http_client_path],
       measure => 'opencensus.io/http/client/received_bytes',
       aggregation => default_size_distribution()},
     #{name => "opencensus.io/http/client/sent_bytes",
       description => "Size distribution of HTTP bytes sent",
       tags => [http_client_method, http_client_path],
       measure => 'opencensus.io/http/client/sent_bytes',
       aggregation => default_size_distribution()},
     #{name => "opencensus.io/http/client/roundtrip_latency",
       description => "Latency distribution of HTTP requests",
       tags => [http_client_method, http_client_path],
       measure => 'opencensus.io/http/client/roundtrip_blatency',
       aggregation => default_latency_distribution()}].

default_size_distribution() ->
    {oc_stat_aggregation_distribution, [{buckets, [0, 1024, 2048, 4096, 16384, 65536,
                                                   262144, 1048576, 4194304, 16777216,
                                                   67108864, 268435456, 1073741824,
                                                   4294967296]}]}.

default_latency_distribution() ->
    {oc_stat_aggregation_distribution, [{buckets, [0, 1, 2, 3, 4, 5, 6, 8, 10, 13, 16, 20, 25, 30,
                                                   40, 50, 65, 80, 100, 130, 160, 200, 250, 300, 400,
                                                   500, 650, 800, 1000, 2000, 5000, 10000, 20000, 50000,
                                                   100000]}]}.
