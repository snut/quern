# Game Design Notes

Notes
 - Slimes: Both player units and initial enemies
 - Board: Simple grid
 - Roguelike/XCOMlike: A (winning) game is a long-ish proposition, with much randomness and critical failure
 - Deck/Army: c.f. Duelyst, Slay the Spire, Mordheim (Necromunda?)

Customisation/team building
 - Differentiate slimes: purpose, abilities, stats, visual
 - 'Army' grows and changes over the course of the game
 - Actions within the combat segments are most important to this development, rather than being consigned to a between-battles management system

Tools/cards
 - Traversal
 - Attacks (melee, aoe, ranged, properties)
 - Summoning?
 - Obstacles?
 - Exciting things to do during the turn-based combat

Board
 - Natural environments
 - Maybe ruins
 - Things to interact with/eat, core of customisation/development system
 - Scale? How big are slimes?
  - v. small? mushrooms = blocking, height variation?
  - tiny leaves on weeds/ferns/mosses
  - leaf litter and grass = big

--

rendering notes

https://www.youtube.com/watch?v=gbeN4lcjJDA (Filament, google rendering project)


tool notes

https://armorpaint.org/download.html

--

Interesting stat spaces: spectrum defined by what units _aren't_ (weaknessess)

e.g.

     tough
    /     \
   /       \
damage --- fast

A given unit may be not-tough, expressing some balance between speed and damage

What does speed mean in a turn-based game? Move+initiative? Crits?
What does tough mean? health/regen/soak/AC/saves

what are the alignments/biases?
- fungal
- vegetable
- mineral
- animal?
- metal
- light
- dark
- earth
- wind/air
- water
- mystic
- anima/animus (Jung's sexist nonsense notwithstanding)

Consideration:
 take d20 ruleset
 remove modifiers: stats range from -10 .. +10 (e.g) [why? so every increment is meaningful]
 nb: dflt proficiency is `max(0,floor((level-1)/4)) + 2`
 double all basic numbers/dice for damage, health, ac, saves etc.
 ?basic check is 2d20, advantage is 3d20k2, disadvantage is 3d20l2
  (this squishes 'viable' AC/DC ranges as the distribution skews to mean 20.5)
  (what is a crit in this?)


--

Friend mentions: what if Guild Wars were a turn based tactical card game
a la Gloomhaven

Some fun archetypes:

prot monk:
- reactive/preparatory damage mitigation
- damage reflection
- buffs

mesmer:
- interruption/disruption
- punish actions (empathy etc) [attack, cast, move...]
- punish inaction (wastrel's)
- pure damage bypassing defenses

necro:
- benefits from things dying nearby
- pets, curses, damage over time
- spiteful stuff
- pretty standard

warrior:
- adrenaline mechanic
- disruption
- wound condition

ranger:
- traps

elementalist:
- pretty boring

**general:**

large/huge number of abilities from two pools
rarity/power restriction (1 elite), see also Gwent bronze/gold/silver



































--
