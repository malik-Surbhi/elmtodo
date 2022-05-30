module Todo exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Browser


type alias Model =
    { todos: List Todo
    , inputText : String
    }

type alias Todo =
    {text : String
    , completed : Bool
    , index : Int
    }

type Message 
    = AddTodo
    | RemoveTodo Int
    | ToggleTodo Int
    | ChangeInput String

init : () -> ( Model, Cmd Message )
init _ =
    ( { inputText = ""
      , todos =[]
      }
    , Cmd.none
    )
update : Message -> Model -> (Model, Cmd Message)
update message model
    = case message of
        AddTodo ->
            ( { model
                | todos =  addToList model.inputText model.todos
                , inputText = ""
            }, Cmd.none )
        RemoveTodo index ->
            ( {model
                |todos = removeFromList index model.todos
            }, Cmd.none )
        ToggleTodo index ->
            ( {model
                |todos = toggleAtIndex index model.todos
            }, Cmd.none )
        ChangeInput input ->
            ( {model
                |inputText = input
            }, Cmd.none )

addToList : String -> List Todo -> List Todo
addToList input todosList =
        todosList ++ [{text = input, completed = False, index = List.length todosList}]

removeFromList : Int -> List Todo -> List Todo
removeFromList index todosList = 
        List.filter  (\item -> item.index /= index) todosList

toggleAtIndex : Int -> List Todo -> List Todo
toggleAtIndex index todosList =
        List.indexedMap (\currentIndex todo -> 
            if todo.index == index then 
                { todo | completed = not todo.completed}
            else
                todo
        ) todosList

view : Model -> Html Message
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div []
                [ h1 [ class "text-center" ] [ text "The ToDo App" ]
                , Html.form []
                    [ div [ class "form-group" ]
                        [ label
                            [ class "control-label"
                            , for "Todo List"
                            ]
                            [ text "Task ToDo" ]
                        , input
                            [ onInput ChangeInput, value model.inputText, class "form-control"
                            , id "name"
                            , type_ "text"
                            ]
                            []
                        ]
                    , div [ class "text-center" ]
                        [ button
                            [ onClick AddTodo, class "btn btn-sm btn-primary add-task"
                            , type_ "button"
                            ]
                            [ text "Add Task" ]
                        ]
                    , div [ class "todo-list" ]
                        [ ul [] 
                            (List.map viewTodo model.todos)
                        ]
                    ]
                ]
            ]
            
        ]

viewTodo : Todo -> Html Message
viewTodo todo =
    li
        [ style "text-decoration"
            ( if todo.completed then
                "line-through"
            else
                "none"
            ),
            class "todo-item" 
        ]
        [ input [ type_ "checkbox", checked todo.completed, onClick (ToggleTodo todo.index), class "checkbox form-check-input" ] []
        , span [class "todo-label"] [text todo.text]
        , i [ class "bi-trash todo-delete", onClick (RemoveTodo todo.index) ] []
        ]

main : Program () Model Message
main =
     Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
