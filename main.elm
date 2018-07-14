module Main exposing (..)

import Csv
import Csv.Decode
import Html exposing (..)
import Html.Events exposing (..)
import Http


type alias Model =
    { universities : List University, students : List Student, studentName : String, studentAge : String, error : Maybe String, doc : Maybe Csv.Csv }


type alias University =
    { name : String, place : Int }


type alias Student =
    { name : String, score : Int, wishes : List University, destination : Maybe University }


type Msg
    = AddStudent
    | UpdateName String
    | UpdateAge String
    | AddWish String String
    | Import
    | UniversitiesImported (Result Http.Error String)
    | Imported (Result Http.Error String)
    | ComputeJury


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( Model [ University "Shanghai" 50, University "Berlin" 42 ] [] "" "" Nothing Nothing, Cmd.none )


updateStudentList : Maybe Student -> Maybe University -> List Student -> List Student
updateStudentList student university students =
    case ( student, university ) of
        ( Just stud, Just univ ) ->
            case students of
                h :: q ->
                    if h == stud then
                        { stud | wishes = univ :: stud.wishes } :: q
                    else
                        h :: updateStudentList student university q

                [] ->
                    []

        _ ->
            students


byName : String -> List { b | name : String } -> Maybe { b | name : String }
byName name list =
    case list of
        t :: q ->
            if t.name == name then
                Just t
            else
                byName name q

        [] ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        AddStudent ->
            ( addStudent model model.studentAge model.studentName, Cmd.none )

        UpdateName name ->
            ( { model | studentName = name }, Cmd.none )

        UpdateAge age ->
            ( { model | studentAge = age }, Cmd.none )

        AddWish student wish ->
            ( { model | students = updateStudentList (byName student model.students) (byName wish model.universities) model.students }, Cmd.none )

        Import ->
            ( model, importUniversities )

        UniversitiesImported (Ok result) ->
            let
                parsedCsv =
                    Csv.Decode.decodeCsv universitiesDecoder (Csv.parse result)
            in
            case parsedCsv of
                Ok parsed ->
                    ( addUniversity model parsed, importAll )

                Err errors ->
                    ( { model | error = Just (toString errors) }, Cmd.none )

        Imported (Ok result) ->
            let
                parsedCsv =
                    Csv.Decode.decodeCsv studentDecoder (Csv.parse result)
            in
            case parsedCsv of
                Ok parsed ->
                    ( addStudents model parsed, Cmd.none )

                Err errors ->
                    ( { model | error = Just (toString errors) }, Cmd.none )

        UniversitiesImported (Err error) ->
            ( { model | error = Just (toString error) }, Cmd.none )

        Imported (Err error) ->
            ( { model | error = Just (toString error) }, Cmd.none )

        ComputeJury ->
            ( computeJury { model | students = [] } model.students, Cmd.none )


computeJury : Model -> List Student -> Model
computeJury model students =
    case students of
        [] ->
            model

        t :: q ->
            let
                student =
                    computeSingleStudentJury model t t.wishes
            in
            computeJury { model | students = student :: model.students } q


computeSingleStudentJury : Model -> Student -> List University -> Student
computeSingleStudentJury model student wishes =
    case wishes of
        [] ->
            student

        t :: q ->
            let
                university =
                    byName t.name model.universities
            in
            case university of
                Nothing ->
                    computeSingleStudentJury model student q

                Just univ ->
                    let
                        currentCount =
                            List.length (List.filter (\x -> x.destination == Just univ) model.students)
                    in
                    if univ.place <= currentCount then
                        computeSingleStudentJury model student q
                    else
                        { student | destination = Just univ }


addStudents : Model -> List Student -> Model
addStudents model students =
    case students of
        [] ->
            model

        s :: q ->
            case byName s.name model.students of
                Nothing ->
                    addStudents (addUniversity { model | students = List.sortBy (\x -> -1 * x.score) (s :: model.students) } s.wishes) q

                Just s ->
                    addStudents model q


addUniversity : Model -> List University -> Model
addUniversity model universities =
    case universities of
        [] ->
            model

        u :: q ->
            case byName u.name model.universities of
                Nothing ->
                    addUniversity { model | universities = u :: model.universities } q

                Just u ->
                    addUniversity model q


buildUniversity : String -> Result String University
buildUniversity name =
    Ok (University name -1)


loginDecoder : Csv.Decode.Decoder (String -> b) b
loginDecoder =
    Csv.Decode.field "login" Ok


choiceDecoder : String -> Csv.Decode.Decoder (University -> b) b
choiceDecoder choiceName =
    Csv.Decode.field choiceName buildUniversity


scoreDecoder : Csv.Decode.Decoder (Int -> b) b
scoreDecoder =
    Csv.Decode.field "score" String.toInt


studentDecoder : Csv.Decode.Decoder (Student -> Student) Student
studentDecoder =
    Csv.Decode.map buildStudent
        (loginDecoder
            |> Csv.Decode.andMap scoreDecoder
            |> Csv.Decode.andMap (choiceDecoder "choix1")
            |> Csv.Decode.andMap (choiceDecoder "choix2")
            |> Csv.Decode.andMap (choiceDecoder "choix3")
            |> Csv.Decode.andMap (choiceDecoder "choix4")
        )


universitiesDecoder : Csv.Decode.Decoder (University -> c) c
universitiesDecoder =
    Csv.Decode.map (\n -> \pl -> University n pl)
        (Csv.Decode.field "name" Ok
            |> Csv.Decode.andMap (Csv.Decode.field "places" String.toInt)
        )


buildStudent : String -> Int -> University -> University -> University -> University -> Student
buildStudent name score c1 c2 c3 c4 =
    Student name score (c1 :: c2 :: c3 :: c4 :: []) Nothing


addStudent : Model -> String -> String -> Model
addStudent model name age =
    case String.toInt age of
        Ok score ->
            { model | students = List.sortBy (\x -> -1 * x.score) (Student model.studentName score [] Nothing :: model.students) }

        Err err ->
            { model | error = Just err }


importAll : Cmd Msg
importAll =
    let
        url =
            "http://localhost/elm/voeux.html"
    in
    let
        req =
            Http.getString url
    in
    Http.send Imported req


importUniversities : Cmd Msg
importUniversities =
    let
        url =
            "http://localhost/elm/universities.html"
    in
    let
        req =
            Http.getString url
    in
    Http.send UniversitiesImported req


displayUniversity : University -> Html Msg
displayUniversity uni =
    Html.text uni.name


displayUniversitySelector : String -> List University -> Html Msg
displayUniversitySelector studentName universities =
    select [ onInput (AddWish studentName) ] (option [] [ text "" ] :: List.map displayOneUniversitySelect universities)


displayOneUniversitySelect : University -> Html Msg
displayOneUniversitySelect univ =
    option [] [ text univ.name ]


displayStudent : Model -> Student -> Html Msg
displayStudent model student =
    li []
        [ Html.text student.name
        , br [] []
        , Html.text (toString student.score)
        , ul [] (List.map displayUniversity student.wishes)
        , displayUniversitySelector student.name model.universities
        ]


displayStudents : Model -> Html Msg
displayStudents model =
    ul [] (List.map (displayStudent model) model.students)


view : Model -> Html Msg
view model =
    div []
        [ displayStudents model
        , input [ onInput UpdateName ] []
        , input [ onInput UpdateAge ] []
        , button [ onClick AddStudent ] [ Html.text "Ajouter" ]
        , button [ onClick Import ] [ Html.text "Importer" ]
        , button [ onClick ComputeJury ] [ Html.text "Classer" ]
        , case model.error of
            Just err ->
                h2 [] [ Html.text err ]

            Nothing ->
                span [] []
        ]
