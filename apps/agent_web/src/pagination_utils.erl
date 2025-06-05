%%%-------------------------------------------------------------------
%%% @doc
%%% Pagination Utilities
%%% Helper functions for implementing pagination across API endpoints
%%% @end
%%%-------------------------------------------------------------------
-module(pagination_utils).

-export([
    parse_pagination_params/1,
    paginate_list/3,
    paginate_list/4,
    format_pagination_response/4,
    get_default_page_size/0,
    get_max_page_size/0
]).

-define(DEFAULT_PAGE_SIZE, 20).
-define(MAX_PAGE_SIZE, 100).

%%%===================================================================
%%% Types
%%%===================================================================
-type pagination_params() :: #{
    page := non_neg_integer(),
    page_size := pos_integer(),
    offset := non_neg_integer()
}.

-type pagination_metadata() :: #{
    page := non_neg_integer(),
    page_size := pos_integer(),
    total_items := non_neg_integer(),
    total_pages := non_neg_integer(),
    has_next := boolean(),
    has_previous := boolean()
}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Parse pagination parameters from HTTP request
-spec parse_pagination_params(cowboy_req:req()) -> pagination_params().
parse_pagination_params(Req) ->
    QsVals = cowboy_req:parse_qs(Req),
    
    Page = case lists:keyfind(<<"page">>, 1, QsVals) of
        {_, PageBin} -> 
            try
                max(1, binary_to_integer(PageBin))
            catch
                _:_ -> 1
            end;
        false -> 1
    end,
    
    PageSize = case lists:keyfind(<<"page_size">>, 1, QsVals) of
        {_, PageSizeBin} ->
            try
                Size = binary_to_integer(PageSizeBin),
                min(max(1, Size), ?MAX_PAGE_SIZE)
            catch
                _:_ -> ?DEFAULT_PAGE_SIZE
            end;
        false -> ?DEFAULT_PAGE_SIZE
    end,
    
    Offset = (Page - 1) * PageSize,
    
    #{
        page => Page,
        page_size => PageSize,
        offset => Offset
    }.

%% @doc Paginate a list with default sorting (by creation time if available)
-spec paginate_list(list(), non_neg_integer(), pos_integer()) -> {list(), pagination_metadata()}.
paginate_list(Items, Offset, PageSize) ->
    paginate_list(Items, Offset, PageSize, fun default_sort/2).

%% @doc Paginate a list with custom sorting function
-spec paginate_list(list(), non_neg_integer(), pos_integer(), function()) -> {list(), pagination_metadata()}.
paginate_list(Items, Offset, PageSize, SortFun) ->
    TotalItems = length(Items),
    TotalPages = ceil(TotalItems / PageSize),
    CurrentPage = (Offset div PageSize) + 1,
    
    % Sort the items
    SortedItems = lists:sort(SortFun, Items),
    
    % Extract the page
    PageItems = case Offset >= TotalItems of
        true -> [];
        false ->
            EndIndex = min(Offset + PageSize, TotalItems),
            lists:sublist(SortedItems, Offset + 1, EndIndex - Offset)
    end,
    
    Metadata = #{
        page => CurrentPage,
        page_size => PageSize,
        total_items => TotalItems,
        total_pages => TotalPages,
        has_next => CurrentPage < TotalPages,
        has_previous => CurrentPage > 1
    },
    
    {PageItems, Metadata}.

%% @doc Format a paginated response for JSON output
-spec format_pagination_response(list(), pagination_metadata(), binary(), map()) -> map().
format_pagination_response(Items, Metadata, ItemsKey, ExtraFields) ->
    BaseResponse = #{
        ItemsKey => Items,
        <<"pagination">> => #{
            <<"page">> => maps:get(page, Metadata),
            <<"page_size">> => maps:get(page_size, Metadata),
            <<"total_items">> => maps:get(total_items, Metadata),
            <<"total_pages">> => maps:get(total_pages, Metadata),
            <<"has_next">> => maps:get(has_next, Metadata),
            <<"has_previous">> => maps:get(has_previous, Metadata)
        }
    },
    maps:merge(BaseResponse, ExtraFields).

%% @doc Get default page size
-spec get_default_page_size() -> pos_integer().
get_default_page_size() ->
    ?DEFAULT_PAGE_SIZE.

%% @doc Get maximum page size
-spec get_max_page_size() -> pos_integer().
get_max_page_size() ->
    ?MAX_PAGE_SIZE.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Default sort function - tries to sort by created_at or updated_at, falls back to comparison
default_sort(A, B) when is_map(A), is_map(B) ->
    case {maps:get(<<"created_at">>, A, undefined), maps:get(<<"created_at">>, B, undefined)} of
        {undefined, undefined} ->
            case {maps:get(<<"updated_at">>, A, undefined), maps:get(<<"updated_at">>, B, undefined)} of
                {undefined, undefined} -> A =< B;
                {undefined, _} -> false;
                {_, undefined} -> true;
                {TimeA, TimeB} -> TimeA >= TimeB  % Most recent first
            end;
        {undefined, _} -> false;
        {_, undefined} -> true;
        {TimeA, TimeB} -> TimeA >= TimeB  % Most recent first
    end;
default_sort(A, B) ->
    A =< B.

%% Safe ceiling function
ceil(X) when X < 0 ->
    trunc(X);
ceil(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.