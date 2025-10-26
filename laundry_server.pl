% laundry_server.pl
% -----------------------------------------------
% SMART LAUNDRY EXPERT PRO — Web Server (WIZARD)
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
% HOME PAGE WITH WIZARD INTERFACE
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
              \wizard_container,
              div(id(results), [])
          ]),
          
          \javascript_code
        ]
    ).

% -----------------------------------------------
% WIZARD CONTAINER HTML
% -----------------------------------------------
wizard_container -->
    html(div(class(wizard_container), [
        % Progress Bar
        div(class(progress_container), [
            div([id(progress_bar), class(progress_bar)], [])
        ]),
        div([id(step_counter), class(step_counter)], 'Step 1 of 8'),
        
        % Wizard Form
        div([id(wizard_form), class(wizard_form)], [
            h2([id(question_title), class(question_title)], ''),
            div([id(question_content), class(question_content)], [])
        ]),
        
        % Navigation Buttons
        div(class(wizard_navigation), [
            button([id(prev_btn), onclick('prevStep()'), class(btn_nav), style('display:none')], 
                   '← Previous'),
            button([id(next_btn), onclick('nextStep()'), class(btn_nav)], 
                   'Next →'),
            button([id(submit_btn), onclick('submitForm()'), class(btn_submit), style('display:none')], 
                   '🔍 Get Recommendation')
        ])
    ])).

% -----------------------------------------------
% JAVASCRIPT CODE FOR WIZARD
% -----------------------------------------------
javascript_code -->
    html(script(type('text/javascript'), [
'
let currentStep = 0;
const answers = {};

const questions = [
    {
        id: "fabric",
        emoji: "👕",
        title: "What type of fabric are you washing?",
        options: [
            {value: "cotton", label: "Cotton", desc: "T-shirts, jeans, bedsheets"},
            {value: "silk", label: "Silk", desc: "Delicate blouses, scarves"},
            {value: "wool", label: "Wool", desc: "Sweaters, blankets"},
            {value: "synthetic", label: "Synthetic", desc: "Polyester, nylon, sportswear"}
        ]
    },
    {
        id: "dirt",
        emoji: "💧",
        title: "How dirty is the laundry?",
        options: [
            {value: "low", label: "Lightly Soiled", desc: "Worn once, freshening up"},
            {value: "medium", label: "Moderately Dirty", desc: "Daily wear, some stains"},
            {value: "high", label: "Heavily Soiled", desc: "Very dirty, muddy, greasy"}
        ]
    },
    {
        id: "color",
        emoji: "🎨",
        title: "What color type is your laundry?",
        options: [
            {value: "white", label: "White", desc: "White clothes only"},
            {value: "colored", label: "Colored", desc: "Bright or light colors"},
            {value: "dark", label: "Dark", desc: "Black, navy, dark colors"}
        ]
    },
    {
        id: "load",
        emoji: "📦",
        title: "How much laundry do you have?",
        options: [
            {value: "small", label: "Small Load", desc: "Few items, 1-2 kg"},
            {value: "medium", label: "Medium Load", desc: "Half machine, 3-5 kg"},
            {value: "large", label: "Large Load", desc: "Full machine, 6+ kg"}
        ]
    },
    {
        id: "stains",
        emoji: "🔴",
        title: "Are there any visible stains?",
        options: [
            {value: "no", label: "No Stains", desc: "Clean or lightly worn"},
            {value: "yes", label: "Yes, Stains Present", desc: "Food, oil, grass, etc."}
        ]
    },
    {
        id: "weather",
        emoji: "☀️",
        title: "What\'s the weather like today?",
        options: [
            {value: "dry", label: "Dry & Sunny", desc: "Good for outdoor drying"},
            {value: "humid", label: "Humid or Rainy", desc: "Indoor drying needed"}
        ]
    },
    {
        id: "urgency",
        emoji: "⏰",
        title: "How urgent is this laundry?",
        options: [
            {value: "low", label: "Not Urgent", desc: "Can wait, normal cycle"},
            {value: "high", label: "Urgent", desc: "Need it soon, quick wash"}
        ]
    },
    {
        id: "water_hardness",
        emoji: "💎",
        title: "What\'s your water hardness?",
        options: [
            {value: "soft", label: "Soft Water", desc: "Easy to lather, no mineral buildup"},
            {value: "hard", label: "Hard Water", desc: "Mineral deposits, harder to lather"}
        ]
    }
];

function displayStep(step) {
    const question = questions[step];
    const questionTitle = document.getElementById("question_title");
    const questionContent = document.getElementById("question_content");
    
    questionTitle.innerHTML = question.emoji + " " + question.title;
    
    let optionsHTML = "<div class=\'options_grid\'>";
    question.options.forEach(option => {
        const isSelected = answers[question.id] === option.value;
        optionsHTML += `
            <div class="option_card ${isSelected ? "selected" : ""}" 
                 onclick="selectOption(\'${question.id}\', \'${option.value}\')">
                <div class="option_label">${option.label}</div>
                <div class="option_desc">${option.desc}</div>
            </div>
        `;
    });
    optionsHTML += "</div>";
    
    questionContent.innerHTML = optionsHTML;
    
    updateProgress();
    updateButtons();
}

function selectOption(questionId, value) {
    answers[questionId] = value;
    
    const cards = document.querySelectorAll(".option_card");
    cards.forEach(card => card.classList.remove("selected"));
    event.currentTarget.classList.add("selected");
    
    document.getElementById("next_btn").disabled = false;
}

function updateProgress() {
    const progress = ((currentStep + 1) / questions.length) * 100;
    document.getElementById("progress_bar").style.width = progress + "%";
    document.getElementById("step_counter").textContent = `Step ${currentStep + 1} of ${questions.length}`;
}

function updateButtons() {
    const prevBtn = document.getElementById("prev_btn");
    const nextBtn = document.getElementById("next_btn");
    const submitBtn = document.getElementById("submit_btn");
    
    prevBtn.style.display = currentStep > 0 ? "inline-block" : "none";
    
    if (currentStep === questions.length - 1) {
        nextBtn.style.display = "none";
        submitBtn.style.display = "inline-block";
    } else {
        nextBtn.style.display = "inline-block";
        submitBtn.style.display = "none";
    }
    
    const currentQuestion = questions[currentStep];
    nextBtn.disabled = !answers[currentQuestion.id];
}

function nextStep() {
    if (currentStep < questions.length - 1) {
        currentStep++;
        displayStep(currentStep);
    }
}

function prevStep() {
    if (currentStep > 0) {
        currentStep--;
        displayStep(currentStep);
    }
}

function submitForm() {
    document.getElementById("results").innerHTML = 
        \'<div class="loading">🔄 Analyzing your laundry... Please wait.</div>\';
    
    const params = new URLSearchParams(answers);
    const url = `/analyze?${params.toString()}`;
    
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

function restartWizard() {
    currentStep = 0;
    Object.keys(answers).forEach(key => delete answers[key]);
    displayStep(0);
    document.getElementById("results").innerHTML = "";
    window.scrollTo({top: 0, behavior: "smooth"});
}

window.onload = function() {
    displayStep(0);
};
'
    ])).

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
    
    assertz(fabric(Fabric)),
    assertz(dirt(Dirt)),
    assertz(color(Color)),
    assertz(load(Load)),
    assertz(stains(Stains)),
    assertz(weather(Weather)),
    assertz(urgency(Urgency)),
    assertz(water_hardness(WaterHardness)),
    
    (mode(Mode) -> true ; Mode = 'No exact match'),
    (temperature(Temp) -> true ; Temp = 'Not determined'),
    (detergent(Det) -> true ; Det = 'Not determined'),
    (drying_method(DryMethod) -> true ; DryMethod = 'Not determined'),
    
    findall(Note, recommendation_note(Note), Notes),
    
    cleanup_facts,
    
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
            
            \display_notes(Notes),
            
            div(class(action_buttons), [
                button([onclick('restartWizard()'), class(btn_new)], 
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

cleanup_facts :-
    retractall(fabric(_)),
    retractall(dirt(_)),
    retractall(color(_)),
    retractall(load(_)),
    retractall(stains(_)),
    retractall(weather(_)),
    retractall(urgency(_)),
    retractall(water_hardness(_)).

:- initialization(server(8080)).
