% laundry_server.pl
% -----------------------------------------------
% SMART LAUNDRY EXPERT PRO — Web Server (FIXED)
% -----------------------------------------------

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).

% Load your knowledge base
:- consult('laundry_expert.pl').

% Define HTTP handlers
:- http_handler(root(.), home_page, []).
:- http_handler(root('analyze'), handle_analysis, []).
:- http_handler(root('static/'), http_reply_from_files('static', []), [prefix]).

% Start the server
server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    format('~n=================================================~n', []),
    format('   SMART LAUNDRY EXPERT PRO - Server Started~n', []),
    format('=================================================~n', []),
    format('   Access the application at:~n', []),
    format('   http://localhost:~w~n', [Port]),
    format('=================================================~n~n', []).

% -----------------------------------------------
% HOME PAGE WITH INTERFACE (FIXED)
% -----------------------------------------------
home_page(_Request) :-
    reply_html_page(
        [title('Smart Laundry Expert Pro'),
         link([rel(stylesheet), href('/static/style.css')])],
        [ 
          div(class(header), [
              h1('🧺 SMART LAUNDRY EXPERT PRO'),
              p('Your AI-powered laundry care advisor')
          ]),
          
          div(class(container), [
              \input_form,
              div(id(results), [])
          ]),
          
          \javascript_code
        ]
    ).

% -----------------------------------------------
% INPUT FORM HTML
% -----------------------------------------------
input_form -->
    html(div(class(form_container), [
        h2(class(form_title), 'Enter Laundry Details'),
        
        % Fabric Type
        div(class(form_group), [
            label([for(fabric)], '👕 Fabric Type:'),
            select([id(fabric), name(fabric), class(input_field)], [
                option([value('cotton')], 'Cotton'),
                option([value('silk')], 'Silk'),
                option([value('wool')], 'Wool'),
                option([value('synthetic')], 'Synthetic')
            ])
        ]),
        
        % Dirt Level
        div(class(form_group), [
            label([for(dirt)], '💧 Dirt Level:'),
            select([id(dirt), name(dirt), class(input_field)], [
                option([value('low')], 'Low'),
                option([value('medium')], 'Medium'),
                option([value('high')], 'High')
            ])
        ]),
        
        % Color Type
        div(class(form_group), [
            label([for(color)], '🎨 Color Type:'),
            select([id(color), name(color), class(input_field)], [
                option([value('white')], 'White'),
                option([value('colored')], 'Colored'),
                option([value('dark')], 'Dark')
            ])
        ]),
        
        % Load Size
        div(class(form_group), [
            label([for(load)], '📦 Load Size:'),
            select([id(load), name(load), class(input_field)], [
                option([value('small')], 'Small'),
                option([value('medium')], 'Medium'),
                option([value('large')], 'Large')
            ])
        ]),
        
        % Stains
        div(class(form_group), [
            label([for(stains)], '🔴 Are there stains?'),
            select([id(stains), name(stains), class(input_field)], [
                option([value('no')], 'No'),
                option([value('yes')], 'Yes')
            ])
        ]),
        
        % Weather
        div(class(form_group), [
            label([for(weather)], '☀️ Current Weather:'),
            select([id(weather), name(weather), class(input_field)], [
                option([value('dry')], 'Dry'),
                option([value('humid')], 'Humid')
            ])
        ]),
        
        % Urgency
        div(class(form_group), [
            label([for(urgency)], '⏰ Is laundry urgent?'),
            select([id(urgency), name(urgency), class(input_field)], [
                option([value('low')], 'Low'),
                option([value('high')], 'High')
            ])
        ]),
        
        % Water Hardness
        div(class(form_group), [
            label([for(water_hardness)], '💎 Water Hardness:'),
            select([id(water_hardness), name(water_hardness), class(input_field)], [
                option([value('soft')], 'Soft'),
                option([value('hard')], 'Hard')
            ])
        ]),
        
        % Submit Button
        button([onclick('getRecommendation()'), class(btn_submit)], 
               '🔍 Get Recommendation')
    ])).

% -----------------------------------------------
% JAVASCRIPT CODE
% -----------------------------------------------
javascript_code -->
    html(script(type('text/javascript'), 
'
function getRecommendation() {
    const fabric = document.getElementById("fabric").value;
    const dirt = document.getElementById("dirt").value;
    const color = document.getElementById("color").value;
    const load = document.getElementById("load").value;
    const stains = document.getElementById("stains").value;
    const weather = document.getElementById("weather").value;
    const urgency = document.getElementById("urgency").value;
    const water_hardness = document.getElementById("water_hardness").value;
    
    document.getElementById("results").innerHTML = 
        \'<div class="loading">🔄 Analyzing your laundry... Please wait.</div>\';
    
    const url = `/analyze?fabric=${fabric}&dirt=${dirt}&color=${color}&load=${load}&stains=${stains}&weather=${weather}&urgency=${urgency}&water_hardness=${water_hardness}`;
    
    fetch(url)
        .then(response => response.text())
        .then(data => {
            document.getElementById("results").innerHTML = data;
            document.getElementById("results").scrollIntoView({ 
                behavior: "smooth", 
                block: "start" 
            });
        })
        .catch(error => {
            console.error("Error:", error);
            document.getElementById("results").innerHTML = 
                \'<div class="error">❌ Error getting recommendation. Please try again.</div>\';
        });
}
'
    )).

% -----------------------------------------------
% HANDLE ANALYSIS REQUEST
% -----------------------------------------------
handle_analysis(Request) :-
    http_parameters(Request, [
        fabric(Fabric, [atom]),
        dirt(Dirt, [atom]),
        color(Color, [atom]),
        load(Load, [atom]),
        stains(Stains, [atom]),
        weather(Weather, [atom]),
        urgency(Urgency, [atom]),
        water_hardness(WaterHardness, [atom])
    ]),
    
    % Assert facts temporarily
    assertz(fabric(Fabric)),
    assertz(dirt(Dirt)),
    assertz(color(Color)),
    assertz(load(Load)),
    assertz(stains(Stains)),
    assertz(weather(Weather)),
    assertz(urgency(Urgency)),
    assertz(water_hardness(WaterHardness)),
    
    % Query the knowledge base
    (mode(Mode) -> true ; Mode = 'No exact match'),
    (temperature(Temp) -> true ; Temp = 'Not determined'),
    (detergent(Det) -> true ; Det = 'Not determined'),
    (drying_method(DryMethod) -> true ; DryMethod = 'Not determined'),
    
    % Collect all recommendations
    findall(Note, recommendation_note(Note), Notes),
    
    % Clean up asserted facts
    cleanup_facts,
    
    % Generate HTML response
    reply_html_page(
        [],
        [ \display_results(Mode, Temp, Det, DryMethod, Notes, 
                          Fabric, Dirt, Color, Load, Stains, 
                          Weather, Urgency, WaterHardness) ]
    ).

% -----------------------------------------------
% DISPLAY RESULTS HTML
% -----------------------------------------------
display_results(Mode, Temp, Det, DryMethod, Notes, 
                Fabric, Dirt, Color, Load, Stains, 
                Weather, Urgency, WaterHardness) -->
    html([
        div(class(results_container), [
            h2(class(results_title), '✨ Your Personalized Laundry Recommendation'),
            
            % Input Summary
            div(class(input_summary), [
                h3('📋 Input Summary'),
                div(class(summary_grid), [
                    div(class(summary_item), [
                        span(class(label), 'Fabric:'), 
                        span(class(value), Fabric)
                    ]),
                    div(class(summary_item), [
                        span(class(label), 'Dirt Level:'), 
                        span(class(value), Dirt)
                    ]),
                    div(class(summary_item), [
                        span(class(label), 'Color:'), 
                        span(class(value), Color)
                    ]),
                    div(class(summary_item), [
                        span(class(label), 'Load Size:'), 
                        span(class(value), Load)
                    ]),
                    div(class(summary_item), [
                        span(class(label), 'Stains:'), 
                        span(class(value), Stains)
                    ]),
                    div(class(summary_item), [
                        span(class(label), 'Weather:'), 
                        span(class(value), Weather)
                    ]),
                    div(class(summary_item), [
                        span(class(label), 'Urgency:'), 
                        span(class(value), Urgency)
                    ]),
                    div(class(summary_item), [
                        span(class(label), 'Water:'), 
                        span(class(value), WaterHardness)
                    ])
                ])
            ]),
            
            % Main Recommendations
            div(class(recommendations), [
                div(class(rec_card), [
                    h4('🔄 Wash Mode'),
                    p(class(rec_value), Mode)
                ]),
                div(class(rec_card), [
                    h4('🌡️ Temperature'),
                    p(class(rec_value), Temp)
                ]),
                div(class(rec_card), [
                    h4('🧴 Detergent'),
                    p(class(rec_value), Det)
                ]),
                div(class(rec_card), [
                    h4('🌬️ Drying Method'),
                    p(class(rec_value), DryMethod)
                ])
            ]),
            
            % Additional Notes
            \display_notes(Notes),
            
            % New Analysis Button
            div(class(action_buttons), [
                button([onclick('window.scrollTo({top: 0, behavior: \\\'smooth\\\'})'), 
                       class(btn_new)], 
                       '🔄 New Analysis')
            ])
        ])
    ]).

display_notes([]) --> html([]).
display_notes(Notes) -->
    { Notes \= [] },
    html(div(class(additional_notes), [
        h3('💡 Additional Recommendations'),
        ul(class(notes_list), \notes_items(Notes))
    ])).

notes_items([]) --> html([]).
notes_items([Note|Rest]) -->
    html([li(Note), \notes_items(Rest)]).

% -----------------------------------------------
% CLEANUP HELPER
% -----------------------------------------------
cleanup_facts :-
    retractall(fabric(_)),
    retractall(dirt(_)),
    retractall(color(_)),
    retractall(load(_)),
    retractall(stains(_)),
    retractall(weather(_)),
    retractall(urgency(_)),
    retractall(water_hardness(_)).

% -----------------------------------------------
% START SERVER ON PORT 8080
% -----------------------------------------------
:- initialization(server(8080)).
