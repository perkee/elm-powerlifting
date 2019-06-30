module View.ColumnToggles exposing (view)

import Bootstrap.Accordion as Accordion
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Grid.Col as Col
import Column
    exposing
        ( Column
        , allColumns
        , columnToToggleLabel
        )
import Data.ColumnToggles
    exposing
        ( Config
        , State
        , toggleColumn
        , updateAccordion
        )
import Html as H exposing (Html)
import Html.Attributes as HA
import Renderer exposing (icon)


view : State -> Config msg -> Html msg
view state (Data.ColumnToggles.Config configRec) =
    if configRec.id == "" then
        H.text ""

    else
        Accordion.config (updateAccordion state >> configRec.toMsg)
            |> Accordion.withAnimation
            |> Accordion.cards
                [ Accordion.card
                    { id = configRec.id
                    , options = []
                    , header =
                        Accordion.toggle [ HA.class "btn-block" ]
                            [ icon
                                (if Accordion.isOpen configRec.id state.accordionState then
                                    "chevron-down"

                                 else
                                    "chevron-up"
                                )
                                [ HA.style "float" "left"
                                , HA.style "padding" ".125em 0 0 0"
                                ]
                            , H.text configRec.title
                            ]
                            |> Accordion.header [ HA.style "padding" "0" ]
                    , blocks =
                        [ Accordion.block []
                            [ Block.titleH4 [] [ H.text "Toggle Fields" ]
                            , Block.text []
                                [ Form.row
                                    []
                                    (allColumns
                                        |> List.map
                                            (columnToToggle configRec.id (\checked -> toggleColumn state checked >> configRec.toMsg) state.columns
                                                >> List.singleton
                                                >> Form.col [ Col.xs6, Col.sm4, Col.md3, Col.lg3 ]
                                            )
                                    )
                                ]
                            ]
                        ]
                    }
                ]
            |> Accordion.view state.accordionState


columnToToggle : String -> (Column -> Bool -> msg) -> List Column -> Column -> Html msg
columnToToggle prefix msg columns col =
    Checkbox.checkbox
        [ col
            |> columnToToggleLabel
            |> (++) prefix
            |> Checkbox.id
        , Checkbox.onCheck <|
            msg col
        , Checkbox.checked <| List.member col columns
        , Checkbox.inline
        ]
        (col |> columnToToggleLabel)
