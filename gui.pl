% robocup.pl



:- use_module(library(random)).
:- use_module(library(pce)).

:- dynamic paused/0.
:- dynamic gui_score_text/1.

pause_gui :- assertz(paused).
resume_gui :- retractall(paused).

% Initialize GUI
start_gui :-
    new(Window, picture('RoboCup GUI')),
    send(Window, size, size(1000, 500)),
    send(Window, open),
    send(Window, display, new(_, text('Score - Team1: 0  Team2: 0')), point(450, 20)),
    assertz(gui_score_text(Window)),
    draw_buttons(Window),
    draw_field(Window),
    assertz(gui_score_text(Window)).

draw_buttons(Window) :-
    send(Window, display, new(PauseBtn, button('Pause', message(@prolog, pause_gui))), point(250, 20)),
    send(Window, display, new(ResumeBtn, button('Resume', message(@prolog, resume_gui))), point(330, 20)).

draw_field(Window) :-
    % Field background
    send(Window, display, new(FieldBox, box(900, 400)), point(50, 50)),
    send(FieldBox, fill_pattern, colour(dark_green)),

    % Thinner white boundary
    send(Window, display, new(Boundary, box(900, 400)), point(50, 50)),
    send(Boundary, pen, 1),
    send(Boundary, colour, white),

    % Midline
    send(Window, display, new(MidLine, line(0, 0, 0, 400)), point(500, 50)),
    send(MidLine, pen, 2),
    send(MidLine, colour, white),

    % Center circle
    send(Window, display, new(Circle, circle(100)), point(450, 200)),
    send(Circle, pen, 2),
    send(Circle, colour, white),

    % Left Goal Area (white lines)
    send(Window, display, new(GoalAreaL, box(50, 200)), point(50, 150)),
    send(GoalAreaL, pen, 2),
    send(GoalAreaL, colour, white),

    % Right Goal Area (white lines)
    send(Window, display, new(GoalAreaR, box(50, 200)), point(900, 150)),
    send(GoalAreaR, pen, 2),
    send(GoalAreaR, colour, white),

    % Left Goal Box (black rectangle) - Adjusted inward
    send(Window, display, new(GoalL, box(10, 100)), point(50, 200)),     
    send(GoalL, fill_pattern, colour(black)),

    % Right Goal Box (black rectangle) - Adjusted outward
    send(Window, display, new(GoalR, box(10, 100)), point(940, 200)),
    send(GoalR, fill_pattern, colour(black)).


    
draw_players(Window) :-
    forall(player(Team, Name, Role, position(X,Y), S), (
        FieldX is X * 9 + 50,
        FieldY is Y * 8 + 50,
        send(Window, display, new(Circle, circle(20)), point(FieldX, FieldY)),
        (Team = team1 -> send(Circle, fill_pattern, colour(blue)) ; send(Circle, fill_pattern, colour(red))),
        send(Window, display, new(_, text(Name)), point(FieldX + 5, FieldY + 2)),
        send(Window, display, new(_, text(S)), point(FieldX + 5, FieldY + 20)),
        send(Window, display, new(_, text(Role)), point(FieldX + 5, FieldY + 35))
    )).


% Update per step
draw_gui_step(Step) :-
    paused, !,
    sleep(1),
    draw_gui_step(Step).

draw_gui_step(Step) :-
    gui_score_text(Window),
    score(team1, S1),
    score(team2, S2),
    format(atom(ScoreText), 'Score - Team1: ~w  Team2: ~w', [S1, S2]),
    send(Window, clear),
    draw_buttons(Window),
    draw_field(Window),
    draw_players(Window),
    draw_ball(Window),
    send(Window, display, new(_, text(ScoreText)), point(450, 20)),
    send(Window, flush),     
    sleep(0.6).              

draw_ball(Window) :-
    ball(position(BX, BY)),
    FieldX is BX * 9 + 50,
    FieldY is BY * 8 + 50,
    send(Window, display, new(B, circle(10)), point(FieldX + 5, FieldY + 5)),
    send(B, fill_pattern, colour(gold)).

update_gui_score :-
    gui_score_text(Window),
    score(team1, S1),
    score(team2, S2),
    format(atom(ScoreText), 'Score - Team1: ~w  Team2: ~w', [S1, S2]),
    send(Window, display, new(_, text(ScoreText)), point(450, 20)).


% Field size
field(size(100, 50)).

% Goal area (horizontal X edge + vertical Y range)
goal_x(team1, 100).
goal_x(team2, 0).
goal_y_range(0, 50).

kick_range(team1, 65).
kick_range(team2, 35).

% Initial ball position
:- dynamic ball/1.
ball(position(50, 25)).

% Possession tracking
:- dynamic possession/2.  % possession(Team, Name)

% --- Initial Player State ---
:- dynamic player/5.

% --- Team 1 (left side) ---
player(team1, gk1,  goalkeeper, position(5, 25),100).
player(team1, def1, defender,   position(20, 15),100).
player(team1, def2, defender,   position(20, 35),100).
player(team1, mid1, midfielder, position(40, 20),100).
player(team1, mid2, midfielder, position(40, 30),100).
player(team1, fwd1, forward,    position(70, 20),100).
player(team1, fwd2, forward,    position(70, 30),100).

% --- Team 2 (right side) ---
player(team2, gk2,  goalkeeper, position(95, 25),100).
player(team2, def1, defender,   position(80, 15),100).
player(team2, def2, defender,   position(80, 35),100).
player(team2, mid1, midfielder, position(60, 20),100).
player(team2, mid2, midfielder, position(60, 30),100).
player(team2, fwd1, forward,    position(30, 20),100).
player(team2, fwd2, forward,    position(30, 30),100).


% Dynamic state

:- dynamic shots/2.
shots(team1, 0).
shots(team2, 0).

:- dynamic shot_in_this_step/1.
:- dynamic score/2.
:- dynamic kickoff_team/1.
score(team1, 0).
score(team2, 0).
kickoff_team(team1).

% --- Initial Positions for Reset ---
initial_position(team1, gk1,  goalkeeper, position(5, 25),100).
initial_position(team1, def1, defender,   position(20, 15),100).
initial_position(team1, def2, defender,   position(20, 35),100).
initial_position(team1, mid1, midfielder, position(40, 20),100).
initial_position(team1, mid2, midfielder, position(40, 30),100).
initial_position(team1, fwd1, forward,    position(70, 20),100).
initial_position(team1, fwd2, forward,    position(70, 30),100).

% Team 2
initial_position(team2, gk2,  goalkeeper, position(95, 25),100).
initial_position(team2, def1, defender,   position(80, 15),100).
initial_position(team2, def2, defender,   position(80, 35),100).
initial_position(team2, mid1, midfielder, position(60, 20),100).
initial_position(team2, mid2, midfielder, position(60, 30),100).
initial_position(team2, fwd1, forward,    position(30, 20),100).
initial_position(team2, fwd2, forward,    position(30, 30),100).




% forward role behavior

% Force rest when stamina is too low
action(forward, rest) :-
    possession(Team, Name),
    player(Team, Name, forward, _, S),
    S < 10, !.

% Force select kick_ball if in range AND near ball (overrides all others)
action(forward, kick_ball) :-
    possession(Team, Name),
    player(Team, Name, forward, position(X, Y), S),
    ball(position(BX, BY)),
    DX is abs(X - BX), DY is abs(Y - BY),
    kick_range(Team, KickX),
    (Team = team1 -> X >= KickX ; X =< KickX),
    (DX =< 3, DY =< 3 ->
        format('[KICK ELIGIBLE] ~w ~w is in range: DX=~w DY=~w Stamina=~w~n', [Team, Name, DX, DY, S])
    ;
        format('[KICK BLOCKED] ~w ~w too far to kick: DX=~w DY=~w~n', [Team, Name, DX, DY]), fail
    ),
    !.



% Dribble when just behind kick range
action(forward, dribble) :-
    possession(Team, Name),
    player(Team, Name, forward, position(X, _), _),
    kick_range(Team, KickX),
    (Team = team1 -> X >= KickX - 5, X < KickX ; X =< KickX + 5, X > KickX),
    !.

% Dribble if in mid zone
action(forward, dribble) :-
    possession(Team, Name),
    player(Team, Name, forward, position(X, _), _),
    (Team = team1 -> X >= 50, X < 65 ; X =< 50, X > 35),
    !.

% Pass if blocked and teammate nearby
action(forward, pass_ball) :-
    possession(Team, Name),
    player(Team, Name, forward, position(X, _), _),
    close_opponent(Team, position(X, _), 3),
    close_teammate(Team, forward, position(X, _), 10),
    !.

% Always allow moving to ball if not yet close
action(forward, move_towards_ball) :-
    \+ possession(_, forward),
    !.

% Smarter shooting/pass/dribble logic

action(Role, rest) :-
    player(Team, Name, Role, _, S),
    S =< 10,
    format('[FORCE REST] ~w ~w (~w) resting at stamina: ~w~n', [Team, Name, Role, S]), !.

% Fallback random action with kick priority
action(forward, Action) :-
    possession(Team, Name),
    player(Team, Name, forward, position(X, Y), _),
    ball(position(BX, BY)),
    DX is abs(X - BX),
    DY is abs(Y - BY),
    % Recheck possession dynamically if near the ball
    ( DX =< 3, DY =< 3 ->
        format('[KICK OVERRIDE] Possession ~w ~w is close to ball at (~w,~w)~n', [Team, Name, BX, BY]),
        Action = kick_ball
    ;
        format('[FALLBACK] forward ~w at (~w,~w), ball at (~w,~w), DX=~w DY=~w~n',
            [Name, X, Y, BX, BY, DX, DY]),
        random_member(Action, [dribble, pass_ball, rest, move_towards_ball])
    ).



% No possession: chase or rest
action(forward, Action) :-
    \+ possession(_, forward),
    random_member(Action, [move_towards_ball, rest]).



% midfielder

action(midfielder, pass_ball) :-
    possession(Team, Name),
    player(Team, Name, midfielder, position(X, Y), _),
    opponent(Team, Opponent),
    goal_x(Opponent, GoalX),
    ( (Team = team1, X >= 60) ; (Team = team2, X =< 40) ),  % mid in attacking zone
    player(Team, ForwardName, forward, position(FX, FY), _),
    abs(FX - X) =< 15, abs(FY - Y) =< 15,  % forward nearby
    format('[PRIORITY PASS] Midfielder ~w sees forward ~w nearby near enemy goal~n', [Name, ForwardName]),
    !.

action(midfielder, rest) :-
    possession(Team, midfielder),
    player(Team, Name, midfielder, _, S),
    S =< 10,
    format('[REST-DECISION] ~w ~w (midfielder) forced to rest at low stamina: ~w~n', [Team, Name, S]),
    !.


action(midfielder, move_towards_ball) :-
    \+ possession(_, midfielder),
    !.

action(midfielder, spread_out) :- 
    possession(_, midfielder),
    !.

action(midfielder, Action) :-
    random_member(Action, [rest, spread_out]).


% defender 
action(defender, Action) :- random_member(Action, [intercept_ball, move_towards_ball, rest, pass_ball]).

% goal keeper
action(goalkeeper, Action) :- random_member(Action, [catch_ball, move_towards_ball, rest]).

% Game step simulation
simulate_step :-
    ( check_goal -> !
    ; simulate_step_logic ).


% Simulate a round with up to MaxSteps steps
simulate_round(MaxSteps) :-
    simulate_round(MaxSteps, 0, false).

simulate_round(Max, Max, false) :-
    format('No goal scored this round.~n~n').
simulate_round(_, _, true).
% Replace simulate_round/3 kickoff clause
simulate_round(Max, Step, Scored) :-
    Step < Max,
    (possession(Team2, Role2) ->
        format('Ball controlled by: ~w ~w~n', [Team2, Role2])
    ; writeln('Ball is free')),
    simulate_step,
    draw_gui_step(Step),
    Step1 is Step + 1,
    simulate_round(Max, Step1, Scored).
    

% Explicit kickoff predicate for clarity
kickoff(Team) :-
    kickoff_team(Team).
% Action implementations
do_action(_, Name, _, S, Action) :-
    S =< 0,
    format('[EXHAUSTED] ~w cannot perform ~w due to 0 stamina.~n', [Name, Action]),
    !.


% Resting
do_action(Team, Name, Role, position(X,Y), S, rest) :-
    NewS is min(100, S + 10),  
    retract(player(Team, Name, Role, position(X, Y), S)),
    assertz(player(Team, Name, Role, position(X, Y), NewS)),
    format('~w ~w (~w) rests and recovers to [Stamina: ~w]~n', [Team, Name, Role, NewS]).


% Move with more diverse Y-axis and realistic motion
do_action(Team, Name, Role, position(X, Y), S, move_towards_ball) :-
    ball(position(BX, BY)),
    Speed = 1.0,
    DX0 is BX - X,
    (S < 50 -> DX = 0 ; sign(DX0, DX)),

    ( random(0.0, 1.0, R), R < 0.3 ->
        random_between(-3, 3, RandY),
        DY is RandY
    ; DY0 is BY - Y,
      % AFTER (allow some Y-axis even when tired)
      (abs(DY0) > 0 -> sign(DY0, SY), DY is round(Speed * SY) ; DY = 0)

    ),

    NewX is X + round(Speed * DX),
    NewY is Y + DY,
    clamp(NewY, 0, 50, FinalY),

    ( Role = goalkeeper ->
        goalkeeper_area(Team, MinX, MaxX),
        NewX >= MinX, NewX =< MaxX
    ; true ),

    NewS is S - 1,
    retract(player(Team, Name, Role, position(X, Y), S)),
    assertz(player(Team, Name, Role, position(NewX, FinalY), NewS)),

    ( ball(position(NewX, FinalY)),
      \+ possession(_, _) ->
        retractall(possession(_, _)),
        assertz(possession(Team, Name)),
        format('[TOUCH PICKUP] ~w ~w takes possession at (~w, ~w)~n', [Team, Name, NewX, FinalY])
    ; true ),

    (NewS < 20 -> format('[TIRED] ~w ~w stamina is low!~n', [Team, Name]) ; true),
    format('~w ~w moves to (~w, ~w) [Stamina: ~w]~n', [Team, Name, NewX, FinalY, NewS]).



% Kick Ball
do_action(Team, Name, forward, position(X,Y), S, kick_ball) :-
    ball(position(BX, BY)),
    DX is abs(X - BX), DY is abs(Y - BY),
    (DX =< 2, DY =< 2 ->
        format('[KICK DEBUG] Forward at (~w, ~w), Ball at (~w, ~w), DX=~w DY=~w~n', [X, Y, BX, BY, DX, DY]),
        format('[KICK] ~w ~w is attempting a kick at (~w, ~w), Stamina: ~w~n', [Team, Name, X, Y, S]),

        increment_shots(Team),
        retractall(shot_in_this_step(_)),
        assertz(shot_in_this_step(Team)),

        (Team == team1 -> GoalX = 100 ; GoalX = 0),
        random_between(0, 100, Roll),
        shot_success_chance(S, SuccessChance),
        format('[KICK] Success roll: ~w <= ~w~n', [Roll, SuccessChance]),

        (Roll =< SuccessChance ->
            random_member(TargetY, [5, 25, 45]),
            random_between(-1, 1, YNoise),
            NewBY is TargetY + YNoise,
            clamp(NewBY, 0, 50, FinalBY),
            NewBX = GoalX,
            format('[GOAL SHOT] Ball headed to goal at (~w, ~w)~n', [NewBX, FinalBY])
        ;
            random_between(-10, 10, MissY),
            clamp(BY + MissY, 0, 50, FinalBY),
            (Team == team1 -> NewBX is min(99, X + 10) ; NewBX is max(1, X - 10)),
            format('[MISS] ~w ~w missed the goal! Reason: Roll (~w) > Chance (~w). Ball veers to (~w, ~w)~n',
                   [Team, Name, Roll, SuccessChance, NewBX, FinalBY])
        ),

        NewS is S - 2,
        retract(ball(position(BX, BY))),
        assertz(ball(position(NewBX, FinalBY))),
        retract(player(Team, Name, forward, position(X,Y), S)),
        assertz(player(Team, Name, forward, position(X,Y), NewS)),
        retractall(possession(_, _)),

        (NewS < 20 -> format('[TIRED] ~w ~w stamina is low!~n', [Team, Name]) ; true),

        (
            \+ check_goal,  % Only apply this fallback if not scored
            (NewBX >= 100 ; NewBX =< 0) ->
            (   % Pass to nearest teammate from defending goalkeeper
                (Team = team1 -> GKTeam = team2, GKX = 95 ; GKTeam = team1, GKX = 5),
                goalkeeper_pass(GKTeam, GKX, FinalBY)
            )
        ; true),

        assign_possession_to_closest
    ;
        format('[KICK FAIL] Ball too far to kick: DX=~w DY=~w~n', [DX, DY]),
        format('[MISS REASON] Cannot kick - ball not within 2-tile radius.~n'),
        format('[NO SHOT COUNTED] ~w ~w attempted kick but was out of range~n', [Team, Name])
    ).





% Dribble
do_action(Team, Name, forward, position(X,Y), S, dribble) :-
    ball(position(BX, BY)),
    abs(X - BX) =< 1, abs(Y - BY) =< 1,
    (Team == team1 -> GoalX = 100 ; GoalX = 0),
    DX is sign(GoalX - X),
    NewX is X + 5 * DX,
    NewS is S - 1,
    retract(player(Team, Name, forward, position(X,Y), S)),
    assertz(player(Team, Name, forward, position(NewX, Y), NewS)),
    retract(ball(position(BX, BY))),
    assertz(ball(position(NewX, Y))),
    retractall(possession(_, _)),
    assertz(possession(Team, Name)),
    (NewS < 20 -> format('[TIRED] ~w ~w stamina is low!~n', [Team, Name]) ; true),
    format('~w ~w dribbles to (~w, ~w)~n', [Team, Name, NewX, Y]).


% Pass Ball
do_action(Team, Name, Role, position(X,Y), S, pass_ball) :-
    possession(Team, Name),
    ball(position(BX, BY)),
    findall(position(TX, TY), (
        player(Team, OtherName, OtherRole, position(TX, TY), _),
        OtherName \= Name
    ), Teammates),
    (Teammates = [] -> format('~w ~w has no teammates to pass to.~n', [Team, Name])
    ;
        random_member(position(TX, TY), Teammates),
        DX is TX - BX, DY is TY - BY,
        sign(DX, SX), sign(DY, SY),
        NewBX is BX + SX, NewBY is BY + SY,
        NewS is S - 1,
        retract(player(Team, Name, Role, position(X,Y), S)),
        assertz(player(Team, Name, Role, position(X,Y), NewS)),
        retract(ball(position(BX, BY))),
        assertz(ball(position(NewBX, NewBY))),
        retractall(possession(_, _)),
        (NewS < 20 -> format('[TIRED] ~w ~w stamina is low!~n', [Team, Name]) ; true),
        format('~w ~w passes the ball toward teammate at (~w, ~w) -> ball moves to (~w, ~w)~n',
            [Team, Name, TX, TY, NewBX, NewBY])
    ).

    
% spreadout    
do_action(Team, Name, midfielder, position(X,Y), S, spread_out) :-
    random_between(-3, 3, DX),
    random_between(-2, 2, DY),
    NewX is X + DX,
    NewY is Y + DY,
    clamp(NewX, 0, 100, FX),
    clamp(NewY, 0, 50, FY),
    NewS is S - 1,
    retract(player(Team, Name, midfielder, position(X,Y), S)),
    assertz(player(Team, Name, midfielder, position(FX, FY), NewS)),
    format('~w ~w spreads out to (~w, ~w) [Stamina: ~w]~n', [Team, Name, FX, FY, NewS]).


% Intercept
do_action(Team, Name, defender, position(X,Y), S, intercept_ball) :-
    ball(position(BX, BY)),
    abs(X - BX) =< 1, abs(Y - BY) =< 1,
    random(0.0, 1.0, Chance), Chance < 0.4,
    NewS is max(0, S - 4),
    retract(player(Team, Name, defender, position(X,Y), S)),
    assertz(player(Team, Name, defender, position(X,Y), NewS)),
    retract(ball(position(BX, BY))),
    assertz(ball(position(X, Y))),
    retractall(possession(_, _)),
    assertz(possession(Team, Name)),
    (NewS < 20 -> format('[TIRED] ~w ~w stamina is low!~n', [Team, Name]) ; true),
    format('~w ~w intercepts and takes control at (~w, ~w)!~n', [Team, Name, X, Y]).

% Catch Ball
do_action(Team, Name, goalkeeper, position(X,Y), S, catch_ball) :-
    ball(position(BX, BY)),
    abs(X - BX) =< 1, abs(Y - BY) =< 1,
    NewS is min(100, S + 2),
    retract(player(Team, Name, goalkeeper, position(X,Y), S)),
    assertz(player(Team, Name, goalkeeper, position(X,Y), NewS)),
    retract(ball(position(BX, BY))),
    assertz(ball(position(X, Y))),
    retractall(possession(_, _)),
    assertz(possession(Team, Name)),
    format('~w ~w catches the ball at (~w, ~w)! [Stamina: ~w]~n', [Team, Name, X, Y, NewS]),
    goalkeeper_pass(Team, X, Y),
    assign_possession_to_closest.


% Chance of successful shot on goal based on stamina
shot_success_chance(Stamina, Chance) :-
    Base is 70,
    (Stamina < 30 -> Penalty is 30 ; Penalty is 0),
    Chance is Base - Penalty.

% Fallback catch-all to catch invalid actions
do_action(Team, Name, Role, _, _, Action) :-
    format('[WARNING] ~w ~w (~w) attempted unsupported or invalid action: ~w~n', [Team, Name, Role, Action]).




% Goalkeeper pass to teammate
goalkeeper_pass(Team, GX, GY) :-
    findall(position(X, Y), (
        player(Team, _, Role, position(X, Y), _),
        Role \= goalkeeper
    ), Teammates),
    (Teammates = [] ->
        format('No teammate to pass to.~n')
    ;
        random_member(position(TX, TY), Teammates),
        DX is TX - GX, DY is TY - GY,
        sign(DX, SX), sign(DY, SY),
        NewBX is GX + SX,
        NewBY is GY + SY,
        retract(ball(position(GX, GY))),
        assertz(ball(position(NewBX, NewBY))),
        format('~w goalkeeper passes toward teammate at (~w, ~w) -> ball moves to (~w, ~w)~n',
            [Team, TX, TY, NewBX, NewBY])
    ).

% goal kick after missed shot
assign_goal_kick(Team) :-
    player(Team, GK, goalkeeper, position(GX, GY), GS),
    retractall(ball(_)),
    assertz(ball(position(GX, GY))),
    retractall(possession(_, _)),
    assertz(possession(Team, GK)),
    NewS is GS - 1,
    retract(player(Team, GK, goalkeeper, position(GX, GY), GS)),
    assertz(player(Team, GK, goalkeeper, position(GX, GY), NewS)),
    format('[GOAL KICK] ~w ~w takes goal kick at (~w,~w)~n', [Team, GK, GX, GY]),
    goalkeeper_pass(Team, GX, GY),
    retractall(missed_shot_team(_)),
    assign_possession_to_closest.




:- dynamic last_ball_y/1.
last_ball_y(25).

goalkeeper_predict_ball(_, _, _, CurrentY, PredY) :-
    last_ball_y(LastY),
    ball(position(_, BY)),
    DeltaY is BY - LastY,
    PredY is CurrentY + DeltaY,
    retractall(last_ball_y(_)),
    assertz(last_ball_y(BY)).

% Goal detection and score update
:- dynamic missed_shot_team/1.
check_goal :-
    ball(position(BX, BY)),
    goal_y_range(MinY, MaxY),
    BY >= MinY, BY =< MaxY,
    (
        BX =< 0,
        shot_in_this_step(team2) ->
            increment_score(team1),
            format('[GOAL] team2 shot into team1 goal!~n'),
            prepare_next_round,
            !
    ;
        BX >= 100,
        shot_in_this_step(team1) ->
            increment_score(team2),
            format('[GOAL] team1 shot into team2 goal!~n'),
            prepare_next_round,
            !
    ),
    !.

check_goal :-
    ball(position(BX, BY)),
    goal_y_range(MinY, MaxY),
    ( BX =< 0 ; BX >= 100 ),
    ( BY < MinY ; BY > MaxY ),
    shot_in_this_step(Team),
    opponent(Team, Opp),
    format('[MISS DETECTED] Shot by ~w missed at (~w, ~w)~n', [Team, BX, BY]),
    retractall(missed_shot_team(_)),
    assertz(missed_shot_team(Opp)),
    assign_goal_kick(Opp),
    !.

check_goal :- false.





increment_score(Team) :-
    score(Team, S),
    NewS is S + 1,
    retract(score(Team, S)),
    assertz(score(Team, NewS)),
    format('GOAL for ~w! Score update: ~w = ~w~n', [Team, Team, NewS]).

prepare_next_round :-
    retractall(ball(_)),
    assertz(ball(position(50, 25))),
    reset_players,
    retractall(possession(_, _)),
    !.


kickoff_pass_to_forward_partner(Team) :-
    player(Team, fwd1, forward, position(FX, FY), FS),
    player(Team, fwd2, forward, position(TX, TY), _),
    retractall(ball(_)),
    assertz(ball(position(FX, FY))),
    retractall(possession(_, _)),
    assertz(possession(Team, fwd1)),

    NewFS is FS - 1,
    retract(player(Team, fwd1, forward, position(FX, FY), FS)),
    assertz(player(Team, fwd1, forward, position(FX, FY), NewFS)),

    DX is TX - FX, DY is TY - FY,
    sign(DX, SX), sign(DY, SY),
    NewBX is FX + SX, NewBY is FY + SY,

    retract(ball(position(FX, FY))),
    assertz(ball(position(NewBX, NewBY))),
    retractall(possession(_, _)),
    assertz(possession(Team, fwd2)),

    format('[KICKOFF PASS] ~w fwd1 passes to fwd2 -> ball moves to (~w, ~w)~n', [Team, NewBX, NewBY]).



switch_kickoff_team :-
    kickoff_team(Current),
    (Current = team1 -> Next = team2 ; Next = team1),
    retract(kickoff_team(Current)),
    assertz(kickoff_team(Next)).

reset_players :-
    forall(initial_position(Team, Name, Role, position(X,Y), Stamina), (
        retractall(player(Team, Name, Role, _, _)),
        assertz(player(Team, Name, Role, position(X,Y), Stamina))
    )).



% Sign function
sign(N, S) :- N > 0, S is 1.
sign(N, S) :- N < 0, S is -1.
sign(0, 0).

clamp(X, Min, Max, R) :-
    ( X < Min -> R = Min
    ; X > Max -> R = Max
    ; R = X).

simulate_step_logic :-
    retractall(shot_in_this_step(_)),
    ball(position(BX, BY)),

    ( \+ possession(_, _) -> assign_possession_to_closest ; true ),

    forall(player(Team, Name, Role, position(X, Y), S), (
        ( possession(Team, Name) ; Role \= goalkeeper, S > 0 ) ->
            (
                ( Role = goalkeeper,
                  possession(Team, Name),
                  goalkeeper_predict_ball(Team, BX, BY, Y, PredY),
                  clamp(PredY, 0, 50, NewY),
                  NewS is S - 1,
                  retract(player(Team, Name, Role, position(_, _), S)),
                  assertz(player(Team, Name, Role, position(X, NewY), NewS)),
                  format('[PREDICT] ~w goalkeeper moves to Y=~w~n', [Team, NewY])
                ; true ),
                action(Role, Action),
                format('~w ~w (~w) chooses to ~w~n', [Team, Name, Role, Action]),
                do_action(Team, Name, Role, position(X, Y), S, Action)
            )
        ; true
    )),

    assign_possession_to_closest,
    update_gui_score.

increment_shots(Team) :-
    shots(Team, S),
    NewS is S + 1,
    retract(shots(Team, S)),
    assertz(shots(Team, NewS)).


% Goalkeeper movement bounds
goalkeeper_area(team1, 0, 10).
goalkeeper_area(team2, 90, 100).

% Run N rounds of up to 50 steps each
run_simulation(0, _) :-
    score(team1, G1), score(team2, G2),
    shots(team1, S1), shots(team2, S2),
    (S1 =:= 0 -> A1 = 0.0 ; A1 is G1 * 100 / S1),
    (S2 =:= 0 -> A2 = 0.0 ; A2 is G2 * 100 / S2),
    format('~nFINAL SCORE: team1 ~w - ~w team2~n', [G1, G2]),
    format('team1 - Shots: ~w, Goals: ~w, Accuracy: ~2f%~n', [S1, G1, A1]),
    format('team2 - Shots: ~w, Goals: ~w, Accuracy: ~2f%~n', [S2, G2, A2]).



run_simulation(N, RoundNum) :-
    format('--- Round ~w ---~n', [RoundNum]),
    prepare_next_round,
    kickoff_team(Team),
    format('[KICKOFF] ~w fwd1 passes to fwd2~n', [Team]),
    (kickoff_pass_to_forward_partner(Team) -> true ; writeln('[KICKOFF FAIL]')),
    simulate_round(50),
    switch_kickoff_team,
    N1 is N - 1,
    R1 is RoundNum + 1,
    run_simulation(N1, R1).



run_simulation(N) :-
    start_gui,
    run_simulation(N, 1).

% Final score report
run_simulation :-
    score(team1, S1),
    score(team2, S2),
    format('Final Score: team1 ~w - ~w team2~n', [S1, S2]).
    
:- dynamic last_possession/2.

assign_possession_to_closest :-
    \+ possession(_, _),
    ball(position(BX, BY)),
    findall(Distance-Team-Name-Role, (
        player(Team, Name, Role, position(X, Y), _),
        Distance is abs(BX - X) + abs(BY - Y),
        Distance =< 3,
        ( \+ last_possession(Team, Role) )  % prevent retaking immediately
    ), Candidates),
    sort(Candidates, Sorted),
    Sorted = [MinDist-Team-Name-Role | _],
    assertz(possession(Team, Name)),
    retractall(last_possession(_, _)),
    assertz(last_possession(Team, Role)),
    format('[AUTO-POSSESSION] Closest player ~w ~w (~w) takes the ball at (~w, ~w)~n', [Team, Name, Role, BX, BY]).
assign_possession_to_closest.

% Teammate nearby (excluding self)
close_teammate(Team, Role, position(X, Y), Radius) :-
    player(Team, OtherName, OtherRole, position(TX, TY), _),
    OtherName \= Role,
    abs(TX - X) =< Radius,
    abs(TY - Y) =< Radius.


% Opponent nearby
close_opponent(Team, position(X, Y), Radius) :-
    nonvar(X), nonvar(Y),
    opponent(Team, Other),
    player(Other, _, _, position(OX, OY), _),
    abs(OX - X) =< Radius,
    abs(OY - Y) =< Radius,
    format('[DEBUG] Opponent nearby blocking kick at (~w,~w)~n', [X, Y]).


  
opponent(team1, team2).
opponent(team2, team1).
