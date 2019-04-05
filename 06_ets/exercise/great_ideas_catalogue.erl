-module(great_ideas_catalogue).

-include_lib("stdlib/include/ms_transform.hrl").

-export([init/0,
         add_idea/5, get_idea/1,
         ideas_by_author/1, ideas_by_rating/1,
         get_authors/0, compare/2]).

-record(idea, {id, title, author, rating, description}).


init() ->
    ets:new(great_ideas_table, [set, named_table, {keypos, 2}]),
    ets:insert(great_ideas_table,
               [#idea{id = 1, title = "Мороженое с огурцами", author = "Боб Бобов", rating = 100,
                      description = "Крошим огурцы кубиками и добавляем в мороженое"},
                #idea{id = 2, title = "Добыча воды на Марсе", author = "Билл Билов", rating = 500,
                      description = "Бурим скважины на Марсе, доставляем воду на Землю ракетами"},
                #idea{id = 3, title = "Извлечение энергии квазаров", author = "П. И. Шурупов", rating = 100500,
                      description = "Секретно"},
                #idea{id = 4, title = "Куртка с тремя рукавами", author = "Боб Бобов", rating = 15,
                      description = "Рукава из разных материалов, расчитаны на разную погоду."},
                #idea{id = 5, title = "Кроссовки-степлеры", author = "Олечка", rating = 78,
                      description = "Полезная вещь для офиса и фитнеса"},
                #idea{id = 6, title = "Способ ловли кузнечиков", author = "Алекс Аквамаринов", rating = 777,
                      description = "Сачком их, сачком."},
                #idea{id = 7, title = "Вулканический зонт", author = "Боб Бобов", rating = 12,
                      description = "Защищает самолеты от вулканической пыли."},
                #idea{id = 8, title = "Телефон-шар", author = "Див Стобс", rating = 8383,
                      description = "Удобно лежит в руке, имеет устройство ввода на основе гироскопа"},
                #idea{id = 9, title = "Автоматическая кормушка для котов", author = "П. И. Шурупов", rating = 9000,
                      description = "Нужно использовать энергию квазаров для этой цели"},
                #idea{id = 10, title = "Самодвижущаяся лестница", author = "Васисуалий Л.", rating = 42,
                      description = "Имеет большой потенциал применения в небоскребах."}]),
    ok.


add_idea(Id, Title, Author, Rating, Description) ->
    Data = #idea{id = Id, title = Title, author = Author, rating = Rating, description = Description},
    ets:insert(great_ideas_table, Data),
    ok.


get_idea(Id) ->
      case ets:lookup(great_ideas_table, Id) of 
            [] -> not_found;
            [Data] -> {ok, Data}
      end.


ideas_by_author(Author) ->
    Pattern = {idea, '$1', '$2', Author, '$4', '$5'},  
    ets:match_object(great_ideas_table, Pattern).


ideas_by_rating(Rating) ->
      MS = ets:fun2ms(
            fun({idea, Id, Title, Author, Rat, Description} = Idea)
                  when Rat >= Rating -> Idea
            end
      ),
      ets:select(great_ideas_table, MS).


get_authors() ->
      MS = ets:fun2ms(
            fun({idea, Id, Title, Author, Rat, Description}) -> Author end
      ),
      Result = ets:select(great_ideas_table, MS),

      Values = lists:foldl(
            fun(Author, Map) ->
                  case maps:find(Author, Map) of 
                        {ok, Count} -> maps:put(Author, Count+1, Map);
                        error -> maps:put(Author, 1, Map)
                  end
            end,
            #{},
            Result
      ),

      lists:sort(fun compare/2, maps:to_list(Values)).


compare({A1, C1}, {A2, C2}) -> 
      case C1 == C2 of 
            true -> A1 < A2;
            _ -> C2 < C1
      end.
