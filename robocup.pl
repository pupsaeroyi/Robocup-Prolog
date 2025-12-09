
:- use_module(library(random)). % For random choices in actions and outcomes.
:- use_module(library(pce)).    % For the visualization of the simulation

% GUI state
:- dynamic window/1.    % Holds the GUI window object.
:- dynamic paused/0.    % Tracks if the game is paused.

% Field size
field(size(100, 50)).   

% Goal area
goal_x(team1, 100).     
goal_x(team2, 0).       
goal_y_range(20, 30).   

% Initial ball position
:- dynamic ball/1.
ball(position(50, 25)). % Ball starts at the center.

% Possession tracking
:- dynamic possession/2. % Tracks who has the ball: possession(Team, Role).

% Player state
:- dynamic player/4.    % Stores player data: player(Team, Role, position(X,Y), stamina)

% Dynamic game state
:- dynamic score/2.                    % Team scores
:- dynamic shot_attempted/1.           % Flags shot attempts
:- dynamic cooldown_after_loss/3.      % Cooldown after losing possession
:- dynamic goalkeeper_pass_target/3.   % Goalkeeper pass target

score(team1, 0).
score(team2, 0).

% Initial player positions with kickoff formations
initial_position(team1_kickoff, team1, forward,    position(45, 25)).  % Team 1 forward close to ball for kickoff
initial_position(team1_kickoff, team1, defender,   position(30, 25)).  % Behind forward
initial_position(team1_kickoff, team1, goalkeeper, position(5, 25)).
initial_position(team1_kickoff, team2, forward,    position(65, 25)).  % Team 2 in defensive position
initial_position(team1_kickoff, team2, defender,   position(80, 25)).  % Behind forward
initial_position(team1_kickoff, team2, goalkeeper, position(95, 25)).

% Team2 Kickoff Formation mirrored positions
initial_position(team2_kickoff, team2, forward,    position(55, 25)).  % Team 2 forward close to ball for kickoff
initial_position(team2_kickoff, team2, defender,   position(70, 25)).  % Behind forward
initial_position(team2_kickoff, team2, goalkeeper, position(95, 25)).
initial_position(team2_kickoff, team1, forward,    position(35, 25)).  % Team 1 in defensive position
initial_position(team2_kickoff, team1, defender,   position(20, 25)).  % Behind forward
initial_position(team2_kickoff, team1, goalkeeper, position(5, 25)).

% Sets up players and possession based on the current formation.
initialize_players :-
    retractall(player(_, _, _, _)),  % clear players
    retractall(possession(_, _)),     % clear possession
    current_formation(Formation),
    % First set up all players
    forall(initial_position(Formation, Team, Role, Position), (
        assertz(player(Team, Role, Position, 100))
    )),
    % Set initial possession based on formation
    (Formation = team1_kickoff ->
        assertz(possession(team1, forward))  % Team 1 starts with possession
    ;
        assertz(possession(team2, forward))  % Team 2 starts with possession
    ).

% forward actions
action(forward, Action) :- 
    ball(position(BX, BY)),
    player(Team, forward, position(X, Y), S),
    (Team = team1 -> GoalX = 100, OppTeam = team2 ; GoalX = 0, OppTeam = team1),
    (
        S =< 30 -> 
            Action = rest
    ;
        possession(Team, goalkeeper) -> 
            find_open_space(Team, X, Y, TargetX, TargetY),
            Action = move_to_position(TargetX, TargetY)
    ;
        possession(OppTeam, _) -> 
            (Team = team1 -> DefensiveX = 40 ; DefensiveX = 60),
            Action = move_to_position(DefensiveX, 25)
    ;
        possession(Team, forward),
        abs(X - BX) =< 1,
        \+ defender_blocking_shot(Team, X, Y),
        abs(X - GoalX) =< 15 -> 
            Action = kick_ball
    ;
        possession(Team, forward),
        abs(X - BX) =< 1,
        \+ defender_blocking_shot(Team, X, Y),
        abs(X - GoalX) =< 30,
        abs(X - GoalX) > 15 -> 
            Action = dribble
    ;
        possession(Team, forward),
        defender_nearby_in_range(Team, X, Y, 3) ->
            random_member(Action, [
                zigzag_then_dribble,
                spin_move,
                fake_left_then_right,
                side_step_then_dribble
            ])

    ;
        possession(Team, forward),
        player(Team, defender, position(DX, DY), _),
        abs(DX - GoalX) < abs(X - GoalX),
        \+ defender_nearby(OppTeam, DX, DY) -> 
            Action = pass_ball
    ;
        possession(Team, forward) -> 
            Action = dribble
    ;
        \+ possession(_, _), 
        abs(X - BX) =< 15 -> 
            Action = move_towards_ball
    ;
        random(0, 2, R), R = 1 -> 
            Action = move_strategic
    ;
        Action = move_towards_ball
    ).




% defender action selection
action(defender, Action) :-
    ball(position(BX, BY)),
    player(Team, defender, position(X, Y), S),
    (Team = team1 -> DangerZone = 40, OppTeam = team2 ; DangerZone = 60, OppTeam = team1),
    (
        S =< 30 ->
            Action = rest
    ;
        possession(OppTeam, forward),
        player(OppTeam, forward, position(OX, OY), _),
        abs(X - OX) =< 5, abs(Y - OY) =< 5,
        (Team = team1, BX < DangerZone ; Team = team2, BX > DangerZone),
        random_between(1, 100, R1),
        R1 =< 40 ->
            random_member(Action, [
                shoulder_charge,
                sliding_tackle
            ])
    ;
        abs(X - BX) =< 3, abs(Y - BY) =< 3,
        \+ possession(_, _) ->
            Action = intercept_ball
    ;
        abs(X - BX) =< 4, abs(Y - BY) =< 4,
        possession(OppTeam, forward) ->
            random_between(1, 100, R2),
            (R2 =< 50 ->
                random_member(Action, [
                    shoulder_charge,
                    sliding_tackle
                ])
            ;
                Action = move_towards_ball
            )
    ;
        % Ensure fallback response if attacker is close
        possession(OppTeam, forward),
        player(OppTeam, forward, position(OX, OY), _),
        abs(X - OX) =< 5, abs(Y - OY) =< 5 ->
            Action = move_towards_ball
    ;
        possession(Team, goalkeeper) ->
            (Team = team1 -> BaseX = 40 ; BaseX = 60),
            find_open_space(Team, BaseX, 25, TargetX, TargetY),
            Action = move_to_position(TargetX, TargetY)
    ;
        possession(Team, defender) ->
            Action = pass_ball
    ;
        possession(Team, _) ->
            Action = move_strategic
    ;
        (Team = team1, BX < DangerZone ; Team = team2, BX > DangerZone) ->
            Action = move_towards_ball
    ;
        Action = move_strategic
    ).



% Action selection for goalkeeper
:- dynamic just_passed/2.

% goalkeeper action 
action(goalkeeper, Action) :-
    ball(position(BX, BY)),
    player(Team, goalkeeper, position(X, Y), S),
    (
        goalkeeper_pass_target(Team, TargetX, TargetY) ->
            format('DEBUG: goalkeeper_pass_target(~w, ~w, ~w) exists~n', [Team, TargetX, TargetY])
    ;
        format('DEBUG: No goalkeeper_pass_target for ~w~n', [Team])
    ),
    (
        goalkeeper_pass_target(Team, TargetX, TargetY),
        TargetX \= -1, TargetY \= -1 ->
            Action = pass_to_teammate(TargetX, TargetY)
    ;
        goalkeeper_pass_target(Team, -1, -1) ->
            Action = goal_kick
    ;
        S =< 20 ->
            Action = rest
    ;
        abs(X - BX) =< 3, abs(Y - BY) =< 3,
        \+ just_passed(Team, _),
        random_between(1, 100, R), R =< 30 ->  % 30% chance to catch
        Action = catch_ball

    ;
        (Team = team1, BX =< 15 ; Team = team2, BX >= 85),
        abs(Y - BY) =< 10 ->
            Action = move_towards_ball
    ;
        S =< 50 ->
            Action = rest
    ;
        possession(Team, goalkeeper) ->
            Action = rest
    ;
        Action = rest
    ).

% Updates cooldowns each simulation step.
tick_cooldowns :-
    forall(cooldown_after_loss(Team, Role, T), (
        T1 is T - 1,
        retract(cooldown_after_loss(Team, Role, T)),
        (T1 > 0 -> assertz(cooldown_after_loss(Team, Role, T1)) ; true)
    )).

% Runs one simulation step
simulate_step :-
    tick_cooldowns,  
    forall(just_passed(Team, N), (
        N1 is N - 1,
        retract(just_passed(Team, N)),
        (N1 > 0 -> assertz(just_passed(Team, N1)) ; true)
    )),
    forall(player(Team, Role, Pos, Stamina), (
        action(Role, Action),
        format('~w ~w chooses to ~w~n', [Team, Role, Action]),
        do_action(Team, Role, Pos, Stamina, Action)
    )).

% Simulate a round up to MaxSteps or until a goal is scored.
simulate_round(MaxSteps) :-
    simulate_round(MaxSteps, 0, false).

simulate_round(Max, Max, false) :-
    format('No goal scored this round.~n~n').
simulate_round(_, _, true).
simulate_round(Max, Step, Scored) :-
    Step < Max,
    (game_ended ->
        true  % Stop simulation if game ended
    ; paused ->
        sleep(0.5),  
        simulate_round(Max, Step, Scored)
    ;
        simulate_step,
        draw_gui_step(Step),
        flush_output,  % Ensure console output is shown
        (check_goal ->
            Step1 is Step + 1,
            format('Goal scored on step ~w~n~n', [Step1]),
            sleep(1),  % Pause after goal
            simulate_round(Max, Step1, true)
        ;
            Step1 is Step + 1,
            simulate_round(Max, Step1, Scored))
    ).

% Action implementations
do_action(_, _, _, S, _) :- S =< 0, !, format('Too tired to act.~n').  % Cannot act if the stamina is depleted

% Rest to recover stamina
do_action(Team, Role, position(X,Y), S, rest) :-     
    NewS is min(100, S + 10), 
    retract(player(Team, Role, position(X,Y), S)),
    assertz(player(Team, Role, position(X,Y), NewS)),
    format('~w ~w rests and recovers to [Stamina: ~w]~n', [Team, Role, NewS]).

% move strategic action
do_action(Team, Role, position(X,Y), S, move_strategic) :-
    ball(position(BX, BY)),
    % Ensure Y is within bounds before proceeding
    (Y < 0 ; Y > 50 ->
        format('Warning: Initial Y position out of bounds (~w) for ~w ~w, clamping to 25~n', [Y, Team, Role]),
        CurrentY = 25
    ;
        CurrentY = Y
    ),
    % Calculate target position based on team and role
    (Team = team1 -> 
        GoalX = 100,
        DefensiveX = 30
    ; 
        GoalX = 0,
        DefensiveX = 70
    ),
    % Calculate target position
    (Role = forward ->
        (possession(Team, _) ->
            % If team has possession, move forward
            TargetX is (X + GoalX) / 2,
            find_space_y(Team, TargetX, BY, TargetY)
        ;   
            % If no possession, move towards ball
            TargetX is BX,
            TargetY is BY
        )
    ; Role = defender ->
        (possession(Team, _) ->
            % If team has possession, move up to support
            TargetX is (BX + DefensiveX) / 2,
            find_space_y(Team, TargetX, BY, TargetY)
        ;   
            % If no possession, stay back
            TargetX is DefensiveX,
            TargetY is 25
        )
    ; Role = goalkeeper ->
        % Goalkeeper stays near goal
        TargetX is (Team = team1 -> 5 ; 95),
        TargetY is 25
    ),
    % Move towards target position
    DX is TargetX - X,
    DY is TargetY - CurrentY,
    Dist is sqrt(DX * DX + DY * DY),
    (Dist > 0 ->
        Speed is 2.0,
        NewX is X + (DX / Dist * Speed),
        NewY is CurrentY + (DY / Dist * Speed)
    ;
        NewX = X,
        NewY = CurrentY
    ),
    % Ensure within bounds
    clamp(NewX, 0, 100, FinalX),
    clamp(NewY, 0, 50, FinalY),
    % Debug log to track Y values
    (FinalY < 0 ; FinalY > 50 ->
        format('Warning: FinalY out of bounds (~w) for ~w ~w after calculation~n', [FinalY, Team, Role])
    ; true),
    % Update position with minimal stamina cost
    StaminaCost is 0.3,  
    NewS is max(0, S - StaminaCost),
    retract(player(Team, Role, position(X,Y), S)),
    assertz(player(Team, Role, position(FinalX, FinalY), NewS)),
    format('~w ~w moves strategically to (~w, ~w) [Stamina: ~w]~n', [Team, Role, FinalX, FinalY, NewS]).

% Helper predicate to find space in Y direction
find_space_y(Team, X, BY, Y) :-
    BaseY is max(10, min(40, BY)),
    findall(OY, (
        player(OtherTeam, _, position(OX, OY), _),
        OtherTeam \= Team,
        abs(X - OX) < 10,
        abs(BaseY - OY) < 10
    ), Opponents),
    (Opponents = [] ->
        Y = BaseY
    ;
        sum_list(Opponents, Sum),
        length(Opponents, Len),
        (Len = 0 ->
            Y = BaseY
        ;
            AvgY is Sum / Len,
            (AvgY > 25 ->
                Y is max(10, BaseY - 10)
            ;
                Y is min(40, BaseY + 10)
            )
        )
    ).

% move towards ball action
do_action(Team, Role, position(X,Y), S, move_towards_ball) :-
    Role \= goalkeeper,  % Exclude goalkeeper role
    ball(position(BX, BY)),
    DX is BX - X,
    DY is BY - Y,
    Speed is 2.0,
    Dist is sqrt(DX * DX + DY * DY),
    (Dist > 0 ->
        NewX is X + (DX / Dist * Speed),
        NewY is Y + (DY / Dist * Speed);
        NewX = X,
        NewY = Y
    ),
    clamp(NewX, 0, 100, FinalX),
    clamp(NewY, 0, 50, FinalY),
    StaminaCost is 0.3,  
    NewS is max(0, S - StaminaCost),
    retract(player(Team, Role, position(X,Y), S)),
    assertz(player(Team, Role, position(FinalX, FinalY), NewS)),

    % Updated possession logic with cooldown check
    (abs(FinalX - BX) =< 3, abs(FinalY - BY) =< 3,
     \+ possession(Team, _),
     \+ possession(_, _),
     \+ cooldown_after_loss(Team, Role, _) ->  % Prevents regaining during cooldown
        retractall(possession(_, _)),
        assertz(possession(Team, Role))
    ; true),

    format('~w ~w moves to (~w, ~w) [Stamina: ~w]~n', [Team, Role, FinalX, FinalY, NewS]).


% forwards dribble
do_action(Team, forward, position(X,Y), S, dribble) :-
    ball(position(BX, BY)),
    possession(Team, forward),
    (Team == team1 -> GoalX = 100 ; GoalX = 0),
    DX0 is GoalX - X,
    Speed is max(1.5, min(3.0, S / 20)),
    random_between(-1, 1, Noise),
    DX is (sign(DX0) * Speed) + (Noise * 0.2),
    findall(position(DX, DY), (
        player(OtherTeam, _, position(DX, DY), _),
        OtherTeam \= Team,
        abs(DX - X) < 10
    ), Defenders),
    (Defenders = [] -> 
        DY is 0
    ;
        nearest_defender(Y, Defenders, AvoidY),
        DY is sign(Y - AvoidY)
    ),
    NewX is X + DX,
    NewY is Y + DY,
    clamp(NewX, 0, 100, FinalX),
    clamp(NewY, 0, 50, FinalY),
    StaminaCost is Speed * 0.8,  
    NewS is max(0, S - StaminaCost),
    retract(player(Team, forward, position(X,Y), S)),
    assertz(player(Team, forward, position(FinalX, FinalY), NewS)),
    BallX is FinalX - sign(DX0) * 0.5,
    BallY is FinalY,
    retract(ball(position(BX, BY))),
    assertz(ball(position(BallX, BallY))),
    format('~w forward dribbles to (~w, ~w) [Stamina: ~w]~n', [Team, FinalX, FinalY, NewS]).

% forwards Zigzag then dribble move 
do_action(Team, forward, position(X,Y), S, zigzag_then_dribble) :-
    random_between(1, 100, Roll),
    (Roll =< 50 ->
        random_between(-1, 1, Dir),
        NewY is Y + Dir * 2,
        clamp(NewY, 0, 50, FinalY),
        (Team = team1 -> DX = 1.5 ; DX = -1.5),
        NewX is X + DX,
        clamp(NewX, 0, 100, FinalX),
        MidS is max(0, S - 1.5),
        retract(player(Team, forward, position(X,Y), S)),
        assertz(player(Team, forward, position(FinalX, FinalY), MidS)),
        format('~w forward zigzags to (~w, ~w) to evade defender! [Stamina: ~w]~n', [Team, FinalX, FinalY, MidS]),
        do_action(Team, forward, position(FinalX, FinalY), MidS, dribble)
    ;
        format('~w forward fumbles zigzag move and loses the ball!~n', [Team]),
        retract(possession(Team, forward))
    ).


% forwards side step then dribble move skill which is more rewarding than zigzag but higher risk of failure
do_action(Team, forward, position(X,Y), S, side_step_then_dribble) :-
    random_between(1, 100, Roll),
    (Roll =< 40 ->
        random_between(-1, 1, Dir),
        NewY is Y + Dir * 3,
        clamp(NewY, 0, 50, FinalY),
        (Team = team1 -> DX = 2.0 ; DX = -2.0),
        NewX is X + DX,
        clamp(NewX, 0, 100, FinalX),
        MidS is max(0, S - 1.5),
        retract(player(Team, forward, position(X,Y), S)),
        assertz(player(Team, forward, position(FinalX, FinalY), MidS)),
        format('~w forward sidesteps to (~w, ~w) to dodge! [Stamina: ~w]~n', [Team, FinalX, FinalY, MidS]),
        do_action(Team, forward, position(FinalX, FinalY), MidS, dribble)
    ;
        format('~w forward fails sidestep and loses the ball!~n', [Team]),
        retract(possession(Team, forward))
    ).

% forwards Fake left then right move
do_action(Team, forward, position(X,Y), S, fake_left_then_right) :-
    random_between(1, 100, Roll),
    (Roll =< 50 ->
        NewY is Y + 1,
        clamp(NewY, 0, 50, FinalY),
        MidS is max(0, S - 1),
        format('~w forward fakes left then cuts right at (~w, ~w) [Stamina: ~w]~n', [Team, X, FinalY, MidS]),
        retract(player(Team, forward, position(X,Y), S)),
        assertz(player(Team, forward, position(X, FinalY), MidS)),
        do_action(Team, forward, position(X, FinalY), MidS, dribble)
    ;
        format('~w forward missteps the fake move and loses the ball!~n', [Team]),
        retract(possession(Team, forward))
    ).

% forwards Spin move 
do_action(Team, forward, position(X,Y), S, spin_move) :-
    random_between(1, 100, Roll),
    (Roll =< 50 ->
        MidS is max(0, S - 2),
        format('~w forward does a 360 spin at (~w, ~w) to shake defender! [Stamina: ~w]~n', [Team, X, Y, MidS]),
        retract(player(Team, forward, position(X,Y), S)),
        assertz(player(Team, forward, position(X,Y), MidS)),
        do_action(Team, forward, position(X,Y), MidS, dribble)
    ;
        format('~w forward slips during spin and loses the ball!~n', [Team]),
        retract(possession(Team, forward))
    ).



% Find the best Y target for shooting based on goalkeeper position
find_best_y_target(Team, GoalX, CurrentBY, TargetY) :-
    (Team = team1 -> OppTeam = team2, GoalY = 25 ; OppTeam = team1, GoalY = 25),
    % Get goalkeeper position
    (player(OppTeam, goalkeeper, position(_, GY), _) ->
        true
    ;
        GY = GoalY  % Default to center if no goalkeeper found
    ),
    goal_y_range(MinY, MaxY),  % Goal range is 20 to 30
    % Calculate possible target Y positions (left, center, right of goal)
    LeftY is MinY + 2,   % Slightly inside the left post
    RightY is MaxY - 2,  % Slightly inside the right post
    CenterY is (MinY + MaxY) / 2,
    % Calculate distances from goalkeeper to each target
    DistLeft is abs(GY - LeftY),
    DistRight is abs(GY - RightY),
    DistCenter is abs(GY - CenterY),
    % Choose the target furthest from the goalkeeper
    (DistLeft >= DistRight, DistLeft >= DistCenter ->
        BestY = LeftY
    ; DistRight >= DistLeft, DistRight >= DistCenter ->
        BestY = RightY
    ;
        BestY = CenterY
    ),
    % Add some variation based on current ball position and accuracy.
    random_between(-1, 1, YVariation),
    TargetY is BestY + YVariation,
    clamp(TargetY, MinY, MaxY, TargetY).

% forwards kick ball action
do_action(Team, forward, position(X,Y), S, kick_ball) :-
    ball(position(BX, BY)),
    possession(Team, forward),
    (abs(X - BX) =< 1, abs(Y - BY) =< 1 ->
        (Team = team1 -> GoalX = 100, Opponent = team2 ; GoalX = 0, Opponent = team1),
        retractall(shot_attempted(_)),
        assertz(shot_attempted(Opponent)),
        find_best_y_target(Team, GoalX, BY, TargetY),
        DX is GoalX - BX,
        DY is TargetY - BY,
        Dist is sqrt(DX*DX + DY*DY),
        StaminaFactor is max(0.8, S / 100),  
        BasePower is max(20, min(35, Dist * 0.8)), 
        Power0 is BasePower * StaminaFactor,
        FinalPower is min(40, max(15, Power0)),
        % accuracy calculation.
        Accuracy is (S / 100) * 0.8 + 0.2,  
        random_between(-2, 2, NoiseY),
        Inaccuracy is NoiseY * (1 - Accuracy),
        NewY is BY + DY / Dist * FinalPower + Inaccuracy,
        NewX is BX + DX / Dist * FinalPower,
        clamp(NewX, 0, 100, FinalX),
        clamp(NewY, 0, 50, FinalY),
        retract(ball(position(BX, BY))),
        assertz(ball(position(FinalX, FinalY))),
        retract(possession(Team, forward)),
        % Slightly reduced stamina cost to compensate for higher power.
        NewS is max(0, S - 4),
        retract(player(Team, forward, position(X,Y), S)),
        assertz(player(Team, forward, position(X,Y), NewS)),
        format('~w forward kicks the ball to (~w, ~w) aiming for Y=~w with power ~1f [Stamina: ~w]~n',
               [Team, FinalX, FinalY, TargetY, FinalPower, NewS])
    ;   % Fallback if conditions fail.
        format('DEBUG: Cannot kick, ball too far (X: ~w, Y: ~w), trying to dribble instead~n', [abs(X - BX), abs(Y - BY)]),
        do_action(Team, forward, position(X,Y), S, dribble)
    ).





% pass ball action
do_action(Team, forward, position(X,Y), S, pass_ball) :-
    possession(Team, forward),
    ball(position(BX, BY)),
    (Team = team1 -> GoalX = 100, OppTeam = team2 ; GoalX = 0, OppTeam = team1),
    % Find best teammate to pass to.
    findall(target(TX, TY, Dist), (
        player(Team, Role, position(TX, TY), _),
        Role \= forward,
        Dist is abs(TX - GoalX),
        \+ defender_nearby(OppTeam, TX, TY)
    ), Targets),
    (Targets \= [] ->
        best_target(Targets, target(TX, TY, _)),
        DX is TX - BX,
        DY is TY - BY,
        Dist is sqrt(DX*DX + DY*DY),
        Power is min(15, max(8, Dist/3)),
        NewBX is BX + (DX/Dist * Power),
        NewBY is BY + (DY/Dist * Power),
        clamp(NewBX, 0, 100, FinalBX),
        clamp(NewBY, 0, 50, FinalBY),
        retract(ball(position(BX, BY))),
        assertz(ball(position(FinalBX, FinalBY))),
        (possession(Team, forward) -> retract(possession(Team, forward)) ; true),
        StaminaCost is min(10, Dist/5),
        NewS is max(0, S - StaminaCost),
        retract(player(Team, forward, position(X,Y), S)),
        assertz(player(Team, forward, position(X,Y), NewS)),
        format('~w forward passes to teammate at (~w, ~w)~n', [Team, TX, TY])
    ;   % If no good target, dribble instead.
        do_action(Team, forward, position(X,Y), S, dribble)
    ).

% pass ball action for defenders
do_action(Team, defender, position(X,Y), S, pass_ball) :-
    possession(Team, defender),
    ball(position(BX, BY)),
    (Team = team1 -> GoalX = 100, OppTeam = team2 ; GoalX = 0, OppTeam = team1),
    % Find best teammate to pass to (prefer forward).
    findall(target(TX, TY, Dist), (
        player(Team, Role, position(TX, TY), _),
        Role = forward,
        Dist is abs(TX - GoalX),
        \+ defender_nearby(OppTeam, TX, TY)
    ), Targets),
    (Targets \= [] ->
        best_target(Targets, target(TX, TY, _)),
        DX is TX - BX,
        DY is TY - BY,
        Dist is sqrt(DX*DX + DY*DY),
        Power is min(15, max(8, Dist/3)),
        NewBX is BX + (DX/Dist * Power),
        NewBY is BY + (DY/Dist * Power),
        clamp(NewBX, 0, 100, FinalBX),
        clamp(NewBY, 0, 50, FinalBY),
        retract(ball(position(BX, BY))),
        assertz(ball(position(FinalBX, FinalBY))),
        (possession(Team, defender) -> retract(possession(Team, defender)) ; true),
        StaminaCost is min(10, Dist/5),
        NewS is max(0, S - StaminaCost),
        retract(player(Team, defender, position(X,Y), S)),
        assertz(player(Team, defender, position(X,Y), NewS)),
        format('~w defender passes to forward at (~w, ~w)~n', [Team, TX, TY])
    ;   % If no good target, pass to any teammate.
        player(Team, Role, position(TX, TY), _),
        player(Team, Role, position(TX, TY), _),
        Role \= defender,
        Role \= goalkeeper,
        DX is TX - BX,
        DY is TY - BY,
        Dist is sqrt(DX*DX + DY*DY),
        Power is min(15, max(8, Dist/3)),
        NewBX is BX + (DX/Dist * Power),
        NewBY is BY + (DY/Dist * Power),
        clamp(NewBX, 0, 100, FinalBX),
        clamp(NewBY, 0, 50, FinalBY),
        retract(ball(position(BX, BY))),
        assertz(ball(position(FinalBX, FinalBY))),
        (possession(Team, defender) -> retract(possession(Team, defender)) ; true),
        StaminaCost is min(10, Dist/5),
        NewS is max(0, S - StaminaCost),
        retract(player(Team, defender, position(X,Y), S)),
        assertz(player(Team, defender, position(X,Y), NewS)),
        format('~w defender passes to teammate at (~w, ~w)~n', [Team, TX, TY])
    ).

% Defender intercepts the ball if close enough.
do_action(Team, defender, position(X,Y), S, intercept_ball) :-
    ball(position(BX, BY)),
    abs(X - BX) =< 2, abs(Y - BY) =< 2,
    retract(ball(position(BX, BY))),
    assertz(ball(position(X, Y))),
    retractall(possession(_, _)),
    assertz(possession(Team, defender)),
    NewS is max(0, S - 1),
    retract(player(Team, defender, position(X,Y), S)),
    assertz(player(Team, defender, position(X,Y), NewS)),

    % Nerf opponent forward harder: longer cooldown
    (Team = team1 -> OppTeam = team2 ; OppTeam = team1),
    retractall(cooldown_after_loss(OppTeam, forward, _)),
    assertz(cooldown_after_loss(OppTeam, forward, 8)),  

    format('~w defender intercepts and takes control at (~w, ~w)!~n', [Team, X, Y]).


% Moves to a specific position used for tactical repositioning.
do_action(Team, Role, position(X,Y), S, move_to_position(TargetX, TargetY)) :-
    % Ensure TargetX and TargetY are instantiated and within bounds.
    number(TargetX), number(TargetY),
    clamp(TargetX, 0, 100, ClampedTargetX),
    clamp(TargetY, 0, 50, ClampedTargetY),
    DX is ClampedTargetX - X,
    DY is ClampedTargetY - Y,
    Dist is sqrt(DX * DX + DY * DY),
    (Dist > 0 ->
        Speed is 2.0,
        NewX is X + (DX / Dist * Speed),
        NewY is Y + (DY / Dist * Speed)
    ;
        NewX = X,
        NewY = Y
    ),
    clamp(NewX, 0, 100, FinalX),
    clamp(NewY, 0, 50, FinalY),
    StaminaCost is 0.3,
    NewS is max(0, S - StaminaCost),
    retract(player(Team, Role, position(X,Y), S)),
    assertz(player(Team, Role, position(FinalX, FinalY), NewS)),
    format('~w ~w moves to position (~w, ~w) [Stamina: ~w]~n', [Team, Role, FinalX, FinalY, NewS]).

% Find an open space to move to and avoid opponents
find_open_space(Team, CurrentX, CurrentY, TargetX, TargetY) :-
    (Team = team1 -> GoalX = 100, SafeX = 50 ; GoalX = 0, SafeX = 50),
    % Define possible target positions (closer to midfield)
    findall(target(TX, TY), (
        member(TY, [15, 25, 35]),  % Left side, Center, Right side
        TX = SafeX
    ), PossibleTargets),
    % Calculate distances to opponents for each target
    findall(Dist-target(TX,TY), (
        member(target(TX, TY), PossibleTargets),
        findall(DistToOpp, (
            player(OtherTeam, _, position(OX, OY), _),
            OtherTeam \= Team,
            DistToOpp is sqrt((TX - OX) * (TX - OX) + (TY - OY) * (TY - OY))
        ), Distances),
        (Distances = [] -> MinDist = 100 ; min_list(Distances, MinDist)),
        Dist = MinDist
    ), DistTargets),
    % Choose the target with the maximum distance from the nearest opponent
    (DistTargets \= [] ->
        sort(DistTargets, Sorted),
        reverse(Sorted, [_-target(TX,TY)|_]),
        TargetX = TX,
        (TY < 0 ; TY > 50 ->
            TargetY = 25  % Default to center if out of bounds
        ;
            TargetY = TY
        )
    ;
        % Default position if no suitable space is found
        TargetX = SafeX,
        TargetY = 25
    ).

% Helper to find minimum value in a list
min_list([H], H).
min_list([H|T], Min) :-
    min_list(T, MinT),
    Min is min(H, MinT).

% Reset players except the goalkeeper who saved to initial positions after a save
reset_players_after_save(SavingTeam) :-
    current_formation(Formation),
    % Retract all players except the goalkeeper of the saving team
    forall(player(Team, Role, _, _), (
        (Team = SavingTeam, Role = goalkeeper) ->
            true  % Keep the goalkeeper who saved
        ;
            retractall(player(Team, Role, _, _))
    )),
    % Reassert players (except the saving goalkeeper) to their initial positions
    forall(initial_position(Formation, Team, Role, position(X,Y)), (
        (Team = SavingTeam, Role = goalkeeper) ->
            true  % Skip the goalkeeper who saved
        ;
            assertz(player(Team, Role, position(X,Y), 100)),
            format('~w ~w resets to initial position (~w, ~w) [Stamina: 100]~n', [Team, Role, X, Y])
    )).

% catch ball action for goalkeepers
do_action(Team, goalkeeper, position(X,Y), S, catch_ball) :-
    ball(position(BX, BY)),
    abs(X - BX) =< 2, abs(Y - BY) =< 2,
    % Clamp position
    clamp(X, 0, 100, ClampedX),
    clamp(Y, 0, 50, ClampedY),
    NewS is min(100, S + 2),

    % Collect teammates before modifying player facts
    findall(target(TX, TY), (
        player(Team, Role, position(TX, TY), _),
        Role \= goalkeeper
    ), Teammates),

    % Update goalie and ball state
    retract(player(Team, goalkeeper, position(X,Y), S)),
    assertz(player(Team, goalkeeper, position(ClampedX, ClampedY), NewS)),
    retract(ball(position(BX, BY))),
    assertz(ball(position(ClampedX, ClampedY))),
    retractall(possession(_, _)),
    assertz(possession(Team, goalkeeper)),

    format('~w goalkeeper catches the ball at (~w, ~w)! [Stamina: ~w]~n', [Team, ClampedX, ClampedY, NewS]),
    
    % log save when shot was attempted
    (shot_attempted(OppTeam),
     OppTeam \= Team ->
        format('~w goalkeeper SAVES the shot at (~w, ~w)!~n', [Team, ClampedX, ClampedY]),
        retractall(shot_attempted(OppTeam))
    ; true),

    % Move teammates and opponents
    forall((player(PlayerTeam, Role, position(PX,PY), PS), Role \= goalkeeper), (
        (PlayerTeam = Team ->
            % Teammate repositioning
            (Role = forward ->
                (Team = team1 -> TX is min(ClampedX + 30, 90) ; TX is max(ClampedX - 30, 10)),
                TY is max(10, min(40, PY))
            ;
                (Team = team1 -> TX is min(ClampedX + 20, 80) ; TX is max(ClampedX - 20, 20)),
                TY = 25
            ),
            retract(player(PlayerTeam, Role, position(PX,PY), PS)),
            assertz(player(PlayerTeam, Role, position(TX, TY), PS)),
            format('~w ~w moves to support position (~w, ~w)~n', [PlayerTeam, Role, TX, TY])
        ;
            % Opponent retreat
            (Role = forward ->
                (Team = team1 -> TX = 60 ; TX = 40)
            ;
                (Team = team1 -> TX = 70 ; TX = 30)
            ),
            TY = 25,
            retract(player(PlayerTeam, Role, position(PX,PY), PS)),
            assertz(player(PlayerTeam, Role, position(TX, TY), PS)),
            format('~w ~w retreats to position (~w, ~w)~n', [PlayerTeam, Role, TX, TY])
        )
    )),
    
    % Trigger passing strategy
    format('DEBUG: Calling choose_goalkeeper_strategy with Teammates = ~w~n', [Teammates]),
    choose_goalkeeper_strategy(Team, ClampedX, ClampedY, NewS, Teammates).


% goal keeper strats
choose_goalkeeper_strategy(Team, X, Y, S, Teammates) :-
    clamp(X, 0, 100, ClampedX),
    clamp(Y, 0, 50, ClampedY),
    findall(target(TX, TY, Dist), (
        member(target(TX, TY), Teammates),
        DX is TX - ClampedX,
        DY is TY - ClampedY,
        DistSquared is DX * DX + DY * DY,
        DistSquared =< 2500,  % 50^2 = 2500
        Dist is sqrt(DistSquared)
    ), NearbyTargets), 
    format('DEBUG: NearbyTargets = ~w~n', [NearbyTargets]),
    (NearbyTargets \= [] ->
        % Sort by distance to find the closest teammate
        sort(3, =<, NearbyTargets, SortedTargets),  % Sort by Dist (third argument of target/3)
        SortedTargets = [target(TX, TY, Dist)|_],
        format('~w goalkeeper chooses to pass to teammate at (~w, ~w) with distance ~w~n', [Team, TX, TY, Dist]),
        retractall(goalkeeper_pass_target(Team, _, _)),
        assertz(goalkeeper_pass_target(Team, TX, TY))
    ;
        format('~w goalkeeper will perform a goal kick due to no teammates found~n', [Team]),
        retractall(goalkeeper_pass_target(Team, _, _)),
        assertz(goalkeeper_pass_target(Team, -1, -1))
    ).

% Pass to a specific teammate
do_action(Team, goalkeeper, position(X,Y), S, pass_to_teammate(TargetX, TargetY)) :-
    format('DEBUG: Entering pass_to_teammate for ~w goalkeeper to (~w, ~w)~n', [Team, TargetX, TargetY]),
    ball(position(BX, BY)),
    possession(Team, goalkeeper),
    DX is TargetX - BX,
    DY is TargetY - BY,
    Dist is sqrt(DX * DX + DY * DY),
    MinSafeDist = 4,
    Power is min(20, max(10, Dist)),
    NewBX is BX + (DX / Dist * Power),
    NewBY is BY + (DY / Dist * Power),
    clamp(NewBX, 0, 100, FinalBX),
    clamp(NewBY, 0, 50, FinalBY),
    retract(ball(position(BX, BY))),
    assertz(ball(position(FinalBX, FinalBY))),
    retractall(possession(_, _)),
    retractall(goalkeeper_pass_target(Team, _, _)),
    StaminaCost is min(5, Dist/5),
    NewS is max(0, S - StaminaCost),
    retract(player(Team, goalkeeper, position(X,Y), S)),
    assertz(player(Team, goalkeeper, position(X,Y), NewS)),
    retractall(just_passed(Team, _)),
    assertz(just_passed(Team, 2)),
    format('~w goalkeeper passes to teammate at (~w, ~w), ball moves to (~w, ~w) [Stamina: ~w]~n',
           [Team, TargetX, TargetY, FinalBX, FinalBY, NewS]).

% goalkeeper goal kick action
do_action(Team, goalkeeper, position(X,Y), S, goal_kick) :-
    ball(position(BX, BY)),
    possession(Team, goalkeeper),
    (Team = team1 -> SafeX = 30 ; SafeX = 70),
    SafeY is 25,
    DX is SafeX - BX,
    DY is SafeY - BY,
    Dist is sqrt(DX * DX + DY * DY),
    Power is max(10, Dist/2),
    NewBX is BX + (DX / Dist * Power),
    NewBY is BY + (DY / Dist * Power),
    clamp(NewBX, 0, 100, FinalBX),
    clamp(NewBY, 0, 50, FinalBY),
    retract(ball(position(BX, BY))),
    assertz(ball(position(FinalBX, FinalBY))),
    retractall(possession(_, _)),
    retractall(goalkeeper_pass_target(Team, _, _)),
    StaminaCost is 5,
    NewS is max(0, S - StaminaCost),
    retract(player(Team, goalkeeper, position(X,Y), S)),
    assertz(player(Team, goalkeeper, position(X,Y), NewS)),
    retractall(just_passed(Team, _)),
    assertz(just_passed(Team, 2)),
    format('~w goalkeeper performs a goal kick to (~w, ~w), ball moves to (~w, ~w) [Stamina: ~w]~n',
           [Team, SafeX, SafeY, FinalBX, FinalBY, NewS]).

% goalkeeper move towards ball action
do_action(Team, goalkeeper, position(X,Y), S, move_towards_ball) :-
    ball(position(BX, BY)),
    goalkeeper_area(Team, MinX, MaxX),
    DX is BX - X,
    DY is BY - Y,
    Speed is 2.0,
    Dist is sqrt(DX * DX + DY * DY),
    (Dist > 0 ->
        NewX is X + (DX / Dist * Speed),
        NewY is Y + (DY / Dist * Speed)
    ;
        NewX = X,
        NewY = Y
    ),
    % Enforce goalkeeper area bounds
    clamp(NewX, MinX, MaxX, FinalX),
    clamp(NewY, 0, 50, FinalY),
    StaminaCost is 0.3,
    NewS is max(0, S - StaminaCost),
    retract(player(Team, goalkeeper, position(X,Y), S)),
    assertz(player(Team, goalkeeper, position(FinalX, FinalY), NewS)),
    format('~w goalkeeper moves to (~w, ~w) [Stamina: ~w]~n', [Team, FinalX, FinalY, NewS]).

% Default fallback for unhandled actions
do_action(_, _, _, _, _).

% Goal detection and score update
check_goal :-
    ball(position(BX, BY)),
    goal_y_range(MinY, MaxY),
    BY >= MinY, BY =< MaxY,
    (
        BX =< 0 -> increment_score(team2)  % Ball in Team1 goal, Team2 scores
    ;
        BX >= 100 -> increment_score(team1)  % Ball in Team2 goal, Team1 scores
    ), !.

% increment score which updates ui
increment_score(Team) :-
    score(Team, S),
    NewS is S + 1,
    retract(score(Team, S)),
    assertz(score(Team, NewS)),
    % Update UI score display
    window(Win),
    update_score(Win),
    send(Win, synchronise),
    format('GOAL for ~w! Score update: ~w = ~w~n', [Team, Team, NewS]).

% Run simulation
run_simulation(0, _) :- 
    format('Simulation complete.~n'),
    window(Win),
    update_score(Win),  % Ensure final score is displayed
    send(Win, synchronise),
    summarize_game_results, !.

% update score
update_score(Win) :-
    score(team1, S1),
    score(team2, S2),
    (get(Win, member, score_display, ScoreText) ->
        format(atom(ScoreStr), 'Score: Team1: ~w - Team2: ~w', [S1, S2]),
        send(ScoreText, string, ScoreStr)
    ;
        new(ScoreText, text('Score: Team1: 0 - Team2: 0')),
        send(ScoreText, font, font(helvetica, bold, 14)),
        send(Win, display, ScoreText, point(280, 20)),
        send(ScoreText, name, score_display)
    ).

% Prepare next round
prepare_next_round :-
    \+ game_ended,
    retract(ball(_)),
    assertz(ball(position(50, 25))),
    retractall(possession(_, _)),
    current_formation(CurrentForm),
    (CurrentForm = team1_kickoff -> 
        retract(current_formation(team1_kickoff)),
        assertz(current_formation(team2_kickoff)),
        assertz(possession(team2, forward))
    ; 
        retract(current_formation(team2_kickoff)),
        assertz(current_formation(team1_kickoff)),
        assertz(possession(team1, forward))
    ),
    reset_players.

reset_players :-
    current_formation(Formation),
    forall(initial_position(Formation, Team, Role, position(X,Y)), (
        retractall(player(Team, Role, _, _)),
        assertz(player(Team, Role, position(X,Y), 100))
    )),
    retractall(possession(_, _)).

:- retractall(initial_position(team1, _, _)).
:- retractall(initial_position(team2, _, _)).

% Sign function
sign(N, S) :- N > 0, S is 1.
sign(N, S) :- N < 0, S is -1.
sign(0, 0).

% Goalkeeper movement bounds
goalkeeper_area(team1, 0, 10).
goalkeeper_area(team2, 90, 100).

% End game state
:- dynamic game_ended/0.

end_game :-
    assertz(game_ended),
    window(Win),
    (get(Win, member, status_text, StatusText) ->
        send(StatusText, string, 'Game Status: Ended')
    ;
        new(StatusText, text('Game Status: Ended')),
        send(StatusText, font, font(helvetica, bold, 12)),
        send(Win, display, StatusText, point(400, 20)),
        send(StatusText, name, status_text)
    ),
    format('Game ended~n'),
    summarize_game_results.

% predicate to show a summary of the game results
summarize_game_results :-
    score(team1, S1),
    score(team2, S2),
    format('~n=== Final Match Results ===~n'),
    format('Team 1: ~w goals~n', [S1]),
    format('Team 2: ~w goals~n', [S2]),
    (S1 > S2 ->
        format('Winner: Team 1~n')
    ; S2 > S1 ->
        format('Winner: Team 2~n')
    ;
        format('Match ended in a draw~n')
    ),
    format('=====================~n~n').

% Run simulation
run_simulation(N, RoundNum) :-
    \+ game_ended,
    format('--- Round ~w ---~n', [RoundNum]),
    simulate_round(100),
    prepare_next_round,
    N1 is N - 1,
    R1 is RoundNum + 1,
    run_simulation(N1, R1).

run_simulation(N) :- 
    retractall(current_formation(_)),
    retractall(goal_kick_state(_)),
    assertz(current_formation(team1_kickoff)),
    initialize_players,
    start_gui,
    format('~nStarting new simulation...~n'),
    format('Number of rounds: ~w~n~n', [N]),
    run_simulation(N, 1).

run_simulation :-
    summarize_game_results.

% Helper predicate to clamp values
clamp(X, Min, Max, R) :-
    ( X < Min -> R = Min
    ; X > Max -> R = Max
    ; R = X).

% GUI 
start_gui :-
    retractall(game_ended),
    (window(Win) -> send(Win, destroy) ; true),
    retractall(window(_)),
    new(Win, picture('RoboCup Simulation')),
    send(Win, size, size(1000, 600)),
    send(Win, open),
    assertz(window(Win)),
    draw_field(Win),
    draw_controls(Win),
    draw_players(Win),
    draw_ball(Win),
    update_score(Win).

draw_field(Win) :-
    new(Field, box(800, 400)),
    send(Field, fill_pattern, colour(dark_green)),
    send(Win, display, Field, point(100, 100)),
    new(Border, box(800, 400)),
    send(Border, pen, 2),
    send(Border, colour, white),
    send(Border, colour, white),
    send(Win, display, Border, point(100, 100)),
    new(CenterLine, line(0, 0, 0, 400)),
    send(CenterLine, colour, white),
    send(CenterLine, pen, 2),
    send(Win, display, CenterLine, point(500, 100)),
    new(Circle, circle(60)),
    send(Circle, pen, 2),
    send(Circle, colour, white),
    send(Win, display, Circle, point(470, 270)),
    new(LeftGoalArea, box(50, 200)),
    send(LeftGoalArea, pen, 2),
    send(LeftGoalArea, colour, white),
    send(Win, display, LeftGoalArea, point(100, 200)),
    new(RightGoalArea, box(50, 200)),
    send(RightGoalArea, pen, 2),
    send(RightGoalArea, colour, white),
    send(Win, display, RightGoalArea, point(850, 200)),
    new(LeftGoal, box(20, 160)),
    send(LeftGoal, fill_pattern, colour(black)),
    send(Win, display, LeftGoal, point(80, 220)),
    new(RightGoal, box(20, 160)),
    send(RightGoal, fill_pattern, colour(black)),
    send(Win, display, RightGoal, point(900, 220)).

draw_controls(Win) :-
    get(Win, width, WinWidth),
    ControlY is 60,
    StartX is 100,  % Left edge of field

    new(PauseButton, button('Pause', message(@prolog, pause_game))),
    send(Win, display, PauseButton, point(StartX, ControlY)),
    get(PauseButton, width, PauseW),

    ResumeX is StartX + PauseW + 10,
    new(ResumeButton, button('Resume', message(@prolog, resume_game))),
    send(Win, display, ResumeButton, point(ResumeX, ControlY)),
    get(ResumeButton, width, ResumeW),

    EndX is ResumeX + ResumeW + 10,
    new(EndButton, button('End Game', message(@prolog, end_game))),
    send(EndButton, colour, red),
    send(Win, display, EndButton, point(EndX, ControlY)),

    % live score report
    new(ScoreText, text('Score: Team1: 0 - Team2: 0')),
    send(ScoreText, font, font(helvetica, bold, 14)),
    get(ScoreText, width, ScoreW),
    ScoreX is (WinWidth - ScoreW) // 2,
    send(Win, display, ScoreText, point(ScoreX, ControlY)),
    send(ScoreText, name, score_display),

    % game status text in gui
    new(StatusText, text('Game Status: Running')),
    send(StatusText, font, font(helvetica, bold, 12)),
    get(StatusText, width, StatusW),
    StatusX is (WinWidth - StatusW) // 2,
    send(Win, display, StatusText, point(StatusX, 520)),
    send(StatusText, name, status_text).


draw_players(Win) :-
    forall(player(Team, Role, position(X, Y), Stamina), (
        ScreenX is 100 + (X * 8),
        ScreenY is 100 + (Y * 8),
        
        % Draw base player circle
        new(P, circle(16)),
        send(P, pen, 2),
        (Team = team1 -> 
            send(P, fill_pattern, colour(royal_blue))
        ; 
            send(P, fill_pattern, colour(red))
        ),
        send(Win, display, P, point(ScreenX - 8, ScreenY - 8)),
        
        % Draw role and stamina
        new(RoleText, text(Role)),
        send(RoleText, font, font(helvetica, bold, 10)),
        send(Win, display, RoleText, point(ScreenX - 15, ScreenY - 30)),
        
        new(StamText, text(Stamina)),
        send(StamText, font, font(helvetica, normal, 10)),
        send(Win, display, StamText, point(ScreenX - 10, ScreenY + 15))
    )).

draw_ball(Win) :-
    ball(position(BX, BY)),
    ScreenX is 100 + (BX * 8),
    ScreenY is 100 + (BY * 8),
    new(Ball, circle(10)),
    send(Ball, fill_pattern, colour(yellow)),
    send(Ball, pen, 2),
    send(Win, display, Ball, point(ScreenX, ScreenY)).

update_gui :-
    window(Win),
    send(Win, clear),
    draw_field(Win),
    draw_controls(Win),
    draw_players(Win),
    draw_ball(Win),
    update_score(Win),
    send(Win, synchronise).

draw_gui_step(Step) :-
    ball(position(BX, BY)),
    update_gui,
    sleep(0.4),
    format('Step ~w - Ball at (~w, ~w)~n', [Step, BX, BY]).

pause_game :- 
    assertz(paused),
    window(Win),
    (get(Win, member, status_text, StatusText) ->
        send(StatusText, string, 'Game Status: Paused')
    ;
        new(StatusText, text('Game Status: Paused')),
        send(StatusText, font, font(helvetica, bold, 12)),
        send(Win, display, StatusText, point(400, 20)),
        send(StatusText, name, status_text)
    ),
    format('Game paused~n').

resume_game :- 
    retractall(paused),
    window(Win),
    (get(Win, member, status_text, StatusText) ->
        send(StatusText, string, 'Game Status: Running')
    ;
        new(StatusText, text('Game Status: Running')),
        send(StatusText, font, font(helvetica, bold, 12)),
        send(Win, display, StatusText, point(400, 20)),
        send(StatusText, name, status_text)
    ),
    format('Game resumed~n').

% Helper predicates
nearest_defender(Y, Defenders, NearestY) :-
    findall(DY, member(position(_, DY), Defenders), YList),
    nearest_value(Y, YList, NearestY).

nearest_value(Y, [DY], DY) :- !.
nearest_value(Y, [DY1,DY2|Rest], Nearest) :-
    abs(Y - DY1) =< abs(Y - DY2),
    nearest_value(Y, [DY1|Rest], Nearest), !.
nearest_value(Y, [DY1,DY2|Rest], Nearest) :-
    nearest_value(Y, [DY2|Rest], Nearest).

count_opponents_between(X1,Y1,X2,Y2,Team,Count) :-
    findall(1, (
        player(OtherTeam, _, position(OX,OY), _),
        OtherTeam \= Team,
        point_in_rectangle(OX,OY, X1,Y1,X2,Y2)
    ), Opponents),
    length(Opponents, Count).

point_in_rectangle(PX,PY, X1,Y1,X2,Y2) :-
    min(X1,X2,MinX), max(X1,X2,MaxX),
    min(Y1,Y2,MinY), max(Y1,Y2,MaxY),
    PX >= MinX, PX =< MaxX,
    PY >= MinY, PY =< MaxY.

min(A,B,A) :- A =< B, !.
min(A,B,B).
max(A,B,A) :- A >= B, !.
max(A,B,B).

best_target([Target], Target) :- !,
    format('DEBUG: best_target base case, Target = ~w~n', [Target]).
best_target([target(X1,Y1,S1),target(X2,Y2,S2)|Rest], Best) :-
    format('DEBUG: best_target comparing S1 = ~w, S2 = ~w~n', [S1, S2]),
    compare_targets(S1, S2, target(X1,Y1,S1), target(X2,Y2,S2), Selected),
    (Rest = [] ->
        Best = Selected
    ;
        best_target([Selected|Rest], Best)
    ).

compare_targets(S1, S2, Target1, _, Target1) :- S1 =< S2, !.
compare_targets(_, _, _, Target2, Target2).

% Goal kick state tracking
:- dynamic goal_kick_state/1.

% Tactical helper predicates
defender_blocking_shot(Team, X, Y) :-
    (Team = team1 ->
        OtherTeam = team2,
        GoalX = 100
    ;
        OtherTeam = team1,
        GoalX = 0
    ),
    player(OtherTeam, defender, position(DX, DY), _),
    point_between(DX, DY, X, Y, GoalX, 25).

defender_nearby(Team, X, Y) :-
    (Team = team1 -> OtherTeam = team2 ; OtherTeam = team1),
    player(OtherTeam, _, position(DX, DY), _),
    abs(X - DX) =< 8,
    abs(Y - DY) =< 8.

defender_nearby_in_range(Team, X, Y, Range) :-
    (Team = team1 -> OtherTeam = team2 ; OtherTeam = team1),
    player(OtherTeam, defender, position(DX, DY), _),
    abs(X - DX) =< Range,
    abs(Y - DY) =< Range.

point_between(PX, PY, X1, Y1, X2, Y2) :-
    min(X1, X2, MinX),
    max(X1, X2, MaxX),
    min(Y1, Y2, MinY),
    max(Y1, Y2, MaxY),
    PX >= MinX, PX =< MaxX,
    PY >= MinY, PY =< MaxY,
    DX1 is X2 - X1,
    DY1 is Y2 - Y1,
    DX2 is PX - X1,
    DY2 is PY - Y1,
    abs(DX1 * DY2 - DX2 * DY1) =< 100.

sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, Rest),
    Sum is H + Rest.

% sliding tackle action for defenders
do_action(Team, defender, position(X,Y), S, sliding_tackle) :-
    ball(position(BX, BY)),
    DX is BX - X,
    DY is BY - Y,
    Dist is sqrt(DX * DX + DY * DY),
    (Dist > 0 ->
        Speed is 4.0,  
        NewX is X + (DX / Dist * Speed),
        NewY is Y + (DY / Dist * Speed)
    ;
        NewX = X,
        NewY = Y
    ),
    clamp(NewX, 0, 100, FinalX),
    clamp(NewY, 0, 50, FinalY),
    StaminaCost is 4.0,
    NewS is max(0, S - StaminaCost),
    retract(player(Team, defender, position(X,Y), S)),
    assertz(player(Team, defender, position(FinalX, FinalY), NewS)),
    (abs(FinalX - BX) =< 1.5, abs(FinalY - BY) =< 1.5 -> 
        retractall(possession(_, _)),
        assertz(possession(Team, defender)),
        retract(ball(position(BX, BY))),
        assertz(ball(position(FinalX, FinalY))),
        % Apply knockback to nearby opponents
        forall(player(OppTeam, Role, position(OX, OY), OS), (
            OppTeam \= Team,
            abs(OX - FinalX) =< 2, abs(OY - FinalY) =< 2 ->
                KnockbackDist is 3.0,
                (Team = team1 ->
                    KnockbackX is OX + KnockbackDist,
                    KnockbackY is OY + (random_between(-1, 1) * 2)
                ;
                    KnockbackX is OX - KnockbackDist,
                    KnockbackY is OY + (random_between(-1, 1) * 2)
                ),
                clamp(KnockbackX, 0, 100, FinalKnockbackX),
                clamp(KnockbackY, 0, 50, FinalKnockbackY),
                retract(player(OppTeam, Role, position(OX, OY), OS)),
                assertz(player(OppTeam, Role, position(FinalKnockbackX, FinalKnockbackY), OS)),
                format('~w ~w knocked back to (~w, ~w) by sliding tackle!~n', 
                       [OppTeam, Role, FinalKnockbackX, FinalKnockbackY])
            ; true
        )),
        format('~w defender performs powerful sliding tackle at (~w, ~w)!~n', [Team, FinalX, FinalY])
    ;
        format('~w defender misses sliding tackle at (~w, ~w)~n', [Team, FinalX, FinalY])
    ).

% Shoulder charge action for defenders
do_action(Team, defender, position(X,Y), S, shoulder_charge) :-
    (Team = team1 -> OppTeam = team2 ; OppTeam = team1),
    player(OppTeam, forward, position(OX, OY), OS),
    DX is OX - X,
    DY is OY - Y,
    Dist is sqrt(DX * DX + DY * DY),
    (Dist > 0 ->
        Speed is 3.5,  
        NewX is X + (DX / Dist * Speed),
        NewY is Y + (DY / Dist * Speed)
    ;
        NewX = X,
        NewY = Y
    ),
    clamp(NewX, 0, 100, FinalX),
    clamp(NewY, 0, 50, FinalY),
    StaminaCost is 3.5,
    NewS is max(0, S - StaminaCost),
    retract(player(Team, defender, position(X,Y), S)),
    assertz(player(Team, defender, position(FinalX, FinalY), NewS)),
    (abs(FinalX - OX) =< 1.5, abs(FinalY - OY) =< 1.5, 
     possession(OppTeam, forward) ->
        random_between(1, 100, R),
        (R =< 60 ->  
            KnockbackDist is 3.0,
            (Team = team1 ->
                KnockbackX is OX + KnockbackDist,
                KnockbackY is OY + (random_between(-1, 1) * 2)
            ;
                KnockbackX is OX - KnockbackDist,
                KnockbackY is OY + (random_between(-1, 1) * 2)
            ),
            clamp(KnockbackX, 0, 100, FinalKnockbackX),
            clamp(KnockbackY, 0, 50, FinalKnockbackY),
            retract(player(OppTeam, forward, position(OX, OY), OS)),
            assertz(player(OppTeam, forward, position(FinalKnockbackX, FinalKnockbackY), OS)),
            retractall(possession(_, _)),
            assertz(possession(Team, defender)),
            format('~w defender wins possession with powerful shoulder charge! Opponent knocked back to (~w, ~w)~n', 
                   [Team, FinalKnockbackX, FinalKnockbackY])
        ;
            format('~w defender attempts shoulder charge but fails~n', [Team])
        )
    ;
        format('~w defender moves to challenge with shoulder charge~n', [Team])
    ).