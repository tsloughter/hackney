%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2014 Benoît Chesneau <benoitc@e-engura.org>
%%%

-module(hackney_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1,
         get_app_env/1, get_app_env/2]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Metrics = metrics:init(hackney_util:mod_metrics()),
  application:set_env(hackney, metrics, Metrics),
  default_views(),
  hackney_sup:start_link().

stop(_State) ->
  ok.

%% @doc return a config value
get_app_env(Key) ->
  get_app_env(Key, undefined).

%% @doc return a config value
get_app_env(Key, Default) ->
  case application:get_env(hackney, Key) of
    {ok, Val} -> Val;
    undefined -> Default
  end.

default_views() ->
    [#{name => "request_count",
       description => "Count of HTTP requests started",
       tags => [],
       measure => 'opencensus.io/http/client/request_count',
       aggregation => oc_stat_count_aggregation},
     #{name => "request_bytes",
       description => "Size distribution of HTTP request body",
       tags => [],
       measure => 'opencensus.io/http/client/request_bytes',
       aggregation => default_size_distribution()},
     #{name => "response_bytes",
       description => "Size distribution of HTTP response body",
       tags => [],
       measure => 'opencensus.io/http/client/response_bytes',
       aggregation => default_size_distribution()},
     #{name => "latency",
       description => "Latency of distribution of HTTP requests",
       tags => [],
       measure => 'opencensus.io/http/client/latency',
       aggregation => default_latency_distribution()},

     #{name => "request_count_by_method",
       description => "Client request count by HTTP method",
       tags => [method],
       measure => 'opencensus.io/http/client/request_count_by_method',
       aggregation => oc_stat_count_aggregation},
     #{name => "response_count_by_status_code",
       description => "Client response count by status code",
       tags => [status_code],
       measure => 'opencensus.io/http/client/response_count_by_status_code',
       aggregation => oc_stat_count_aggregation}].

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
