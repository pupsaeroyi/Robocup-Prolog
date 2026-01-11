RoboCup Prolog Simulation
A dynamic soccer simulation built in SWI-Prolog featuring rule-based AI players (agents) and real-time GUI visualization.

Overview
This project implements a complete soccer simulation in Prolog with intelligent agent-based players. 
Each team consists of 3 players (goalkeeper, defender, forward) that make autonomous decisions based on the game state, 
stamina levels, and tactical positioning.
The simulation features:
Real-time GUI using XPCE library for visualization
Dynamic AI with role-specific behaviors and tactics
Stamina management affecting player performance
Advanced skills including dribbling, passing, tackling, and special moves
Strategic positioning with offensive and defensive formations

Features
Gameplay
Full 3v3 soccer matches with realistic ball physics
Real-time simulation with adjustable speed
Live score tracking and statistics
Multiple rounds with kickoff rotation
Pause/Resume/End game controls

Player Intelligence
Goalkeepers: Predictive positioning, catching, and distribution
Defenders: Interception, tackling (sliding/shoulder charge), strategic positioning
Forwards: Dribbling, shooting, skill moves (zigzag, spin, fake moves)

Advanced Mechanics
Stamina system affecting speed and accuracy
Shot accuracy based on distance and stamina
Team coordination with passing strategies
Defensive tactics including blocking and cooldowns
Skill moves with success/failure probabilities

Game Mechanics
Field Layout
Dimensions: 100×50 units
Goals: Located at X=0 (Team 2) and X=100 (Team 1)
Goal Height: Y=20 to Y=30
Center Circle: Radius of 30 units

Scoring System
Goals scored when ball crosses goal line within Y-range
Missed shots result in goal kicks
Kickoff alternates between teams after each goal

Stamina System
Players start with 100 stamina
Actions consume stamina (movement: 0.3, kick: 4, tackle: 4)
Resting recovers 10 stamina per tick
Low stamina reduces speed and shot accuracy

Possession Mechanics
Automatic possession when player is within 3 units of ball
Cooldown period after losing possession (8 ticks)
Interception and tackling can force possession changes
