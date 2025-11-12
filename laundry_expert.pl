% -----------------------------------------------
% SMART LAUNDRY EXPERT PRO — Knowledge Base
% -----------------------------------------------

% --- Dynamic facts ---
:- dynamic fabric/1, dirt/1, color/1, load/1, stains/1, weather/1, urgency/1, water_hardness/1.

% -----------------------------------------------
% INTERMEDIATE REASONING LAYERS
% -----------------------------------------------

% 1️⃣ Determine fabric care level
fabric_care(delicate) :- fabric(silk).
fabric_care(delicate) :- fabric(wool).
fabric_care(normal) :- fabric(cotton).
fabric_care(normal) :- fabric(synthetic).

% 2️⃣ Determine washing intensity
intensity(low) :- dirt(low), stains(no).
intensity(medium) :- dirt(medium).
intensity(medium) :- stains(yes), dirt(low).
intensity(high) :- dirt(high).
intensity(high) :- stains(yes), dirt(medium).

% 3️⃣ Determine water temperature preference
temperature(cold) :- fabric_care(delicate).
temperature(warm) :- intensity(medium), color(colored).
temperature(hot)  :- intensity(high), fabric(cotton).

% 4️⃣ Recommend detergent type
detergent(liquid) :- fabric_care(delicate).
detergent(powder) :- intensity(high).
detergent(eco_friendly) :- color(white), dirt(low), water_hardness(soft).
detergent(anti_stain) :- stains(yes), intensity(high).

% 5️⃣ Determine drying method
drying_method(line_dry) :- fabric_care(delicate).
drying_method(line_dry) :- color(dark).
drying_method(tumble_dry) :- fabric_care(normal), intensity(high), weather(dry).
drying_method(indoor_dry) :- weather(humid).

% 6️⃣ Select washing mode
mode(quick_wash) :-
    intensity(low),
    load(small),
    urgency(high).

mode(delicate_cycle) :-
    fabric_care(delicate),
    intensity(low).

mode(eco_wash) :-
    dirt(low),
    load(medium),
    water_hardness(soft).

mode(normal_wash) :-
    intensity(medium),
    fabric_care(normal).

mode(heavy_duty) :-
    intensity(high),
    load(large).

% 7️⃣ Additional recommendations
recommendation_note('Use fabric softener.') :-
    fabric(wool).
recommendation_note('Use fabric softener.') :-
    fabric(cotton).

recommendation_note('Avoid bleach to preserve colors.') :-
    color(colored).
recommendation_note('Avoid bleach to preserve colors.') :-
    color(dark).

recommendation_note('Pre-treat stained areas before washing.') :-
    stains(yes).

recommendation_note('Consider warm drying to avoid mildew.') :-
    weather(humid).

% -----------------------------------------------
% USER INTERFACE
% -----------------------------------------------

start :-
    write('--- SMART LAUNDRY EXPERT PRO ---'), nl,
    write('Please answer the following questions:'), nl,
    ask(fabric, 'Fabric type (cotton/silk/wool/synthetic): '),
    ask(dirt, 'Dirt level (low/medium/high): '),
    ask(color, 'Color type (white/colored/dark): '),
    ask(load, 'Load size (small/medium/large): '),
    ask(stains, 'Are there stains? (yes/no): '),
    ask(weather, 'Current weather (dry/humid): '),
    ask(urgency, 'Is laundry urgent? (high/low): '),
    ask(water_hardness, 'Water hardness (soft/hard): '),

    nl, write('--- RECOMMENDATION ---'), nl,

    (mode(M) -> write('Wash Mode: '), write(M), nl ; write('Wash Mode: No exact match.'), nl),
    (temperature(T) -> write('Temperature: '), write(T), nl ; write('Temperature: Not found.'), nl),
    (detergent(D) -> write('Detergent: '), write(D), nl ; write('Detergent: Not found.'), nl),
    (drying_method(DM) -> write('Drying Method: '), write(DM), nl ; write('Drying Method: Not found.'), nl),

    nl, write('Additional Recommendations:'), nl,
    forall(recommendation_note(Note), (write('- '), write(Note), nl)),

    nl, write('End of analysis. Thank you for using SMART LAUNDRY EXPERT PRO!'), nl,

    undo.

% -----------------------------------------------
% Helper predicates
% -----------------------------------------------

ask(Attr, Prompt) :-
    write(Prompt),
    read(Value),
    Fact =.. [Attr, Value],
    assertz(Fact).

% Cleanup
undo :- retract(fabric(_)), fail.
undo :- retract(dirt(_)), fail.
undo :- retract(color(_)), fail.
undo :- retract(load(_)), fail.
undo :- retract(stains(_)), fail.
undo :- retract(weather(_)), fail.
undo :- retract(urgency(_)), fail.
undo :- retract(water_hardness(_)), fail.
undo.

