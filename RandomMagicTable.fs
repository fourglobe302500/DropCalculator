module DropCalculator.RandomMagicTable

open Extra

let many tables random =
    tables |> List.collect (fun f -> f random)

let createFixed nome raridade valor =
    { Name = nome
      Amount = Fixed 1
      Condition = Base
      Rarity = raridade
      Size = Medium
      Value = (Gold, valor) }

let A count random =
    let roll = roll random

    [ for _ in 1 .. roll 1 count ->
          match roll 1 100 with
          | Range 1 50 -> createFixed "Potion of Healing" Common 50u
          | Range 51 60 -> createFixed "Spell Scroll(Cantrip)" Common 10u
          | Range 61 70 -> createFixed "Potion of Climbing" Common 180u
          | Range 71 90 -> createFixed "Spell Scroll(1st level)" Common 60u
          | Range 91 94 -> createFixed "Spell Scroll(2nd level)" Uncommon 120u
          | Range 95 98 -> createFixed "Potion of Greater Healing" Uncommon 200u
          | 99 -> createFixed "Bag of Holding" Uncommon 4000u
          | _ -> createFixed "Drift globe" Uncommon 750u ]

let B count random =
    let roll = roll random

    [ for _ in 1 .. roll 1 count ->
          match roll 1 100 with
          | Range 1 15 -> createFixed "Potion of Greater Healing" Uncommon 200u
          | Range 16 22 -> createFixed "Potion of Fire Breath" Uncommon 50u
          | Range 23 29 -> createFixed "Potion of Resistance" Uncommon 300u
          | Range 30 34 -> createFixed "Ammunition +1" Uncommon 25u
          | Range 35 39 -> createFixed "Potion of Animal Friendship" Uncommon 200u
          | Range 40 44 -> createFixed "Potion of Giant Strength(Hill Giant)" Uncommon 400u
          | Range 45 49 -> createFixed "Potion of Growth" Uncommon 270u
          | Range 50 54 -> createFixed "Potion of Water Breathing" Uncommon 180u
          | Range 55 59 -> createFixed "Spell Scroll(2nd level)" Uncommon 120u
          | Range 60 64 -> createFixed "Spell Scroll(3rd level)" Uncommon 200u
          | Range 65 67 -> createFixed "Bag of Holding" Uncommon 4000u
          | Range 68 70 -> createFixed "Keoghtom's Ointment" Uncommon 120u
          | Range 71 73 -> createFixed "Oil of Slipperiness" Uncommon 480u
          | Range 74 75 -> createFixed "Dust of Disappearance" Uncommon 300u
          | Range 76 77 -> createFixed "Dust of Dryness" Uncommon 120u
          | Range 78 79 -> createFixed "Dust of Sneezing and Choking" Uncommon 480u
          | Range 80 81 -> createFixed "Elemental Gem" Uncommon 960u
          | Range 82 83 -> createFixed "Philter of Love" Uncommon 90u
          | 84 -> createFixed "Alchemy Jug" Uncommon 6000u
          | 85 -> createFixed "Cap of Water Breathing" Uncommon 500u
          | 86 -> createFixed "Cloak of the Manta Ray" Uncommon 6000u
          | 87 -> createFixed "Drift globe" Uncommon 750u
          | 88 -> createFixed "Goggles of Night" Uncommon 1500u
          | 89 -> createFixed "Helm of Comprehending Languages" Uncommon 500u
          | 90 -> createFixed "Immovable Rod" Common 5000u
          | 91 -> createFixed "Lantern of Revealing" Uncommon 5000u
          | 92 -> createFixed "Mariner's Armor" Uncommon 1500u
          | 93 ->
              match roll 1 8 with
              | 1 -> createFixed "Mithral Chain Shirt" Uncommon 850u
              | 2 -> createFixed "Mithral Scale Mail" Uncommon 850u
              | 3 -> createFixed "Mithral Breastplate" Uncommon 1200u
              | 4 -> createFixed "Mithral Half Plate" Uncommon 1550u
              | 5 -> createFixed "Mithral Ring Mail" Uncommon 830u
              | 6 -> createFixed "Mithral Chain Mail" Uncommon 875u
              | 7 -> createFixed "Mithral Splint" Uncommon 1000u
              | _ -> createFixed "Mithral Plate" Uncommon 2300u
          | 94 -> createFixed "Potion of Poison" Uncommon 100u
          | 95 -> createFixed "Ring of Swimming" Uncommon 3000u
          | 96 -> createFixed "Robe of Useful Items" Uncommon 500u
          | 97 -> createFixed "Rope of Climbing" Uncommon 2000u
          | 98 -> createFixed "Saddle of the Cavalier" Uncommon 2000u
          | 99 -> createFixed "Wand of Magic Detection" Uncommon 1500u
          | _ -> createFixed "Wand of Secrets" Uncommon 1500u ]

let C count random =
    let roll = roll random

    [ for _ in 1 .. roll 1 count ->
          match roll 1 100 with
          | Range 1 15 -> createFixed "Potion of Superior Healing" Rare 500u
          | Range 16 22 -> createFixed "Spell Scroll (Level 4)" Rare 320u
          | Range 23 27 -> createFixed "Ammunition +2" Rare 100u
          | Range 28 32 -> createFixed "Potion of Clairvoyance" Rare 960u
          | Range 33 37 -> createFixed "Potion of Diminution" Rare 270u
          | Range 38 42 -> createFixed "Potion of Gaseous Form" Rare 300u
          | Range 43 47 -> createFixed "Potion of Giant Strength (Frost Giant)" Rare 500u
          | Range 48 52 -> createFixed "Potion of Giant Strength (Stone Giant)" Rare 500u
          | Range 53 57 -> createFixed "Potion of Heroism" Rare 180u
          | Range 58 62 -> createFixed "Potion of Invulnerability" Rare 3840u
          | Range 63 67 -> createFixed "Potion of Mind Reading" Rare 180u
          | Range 68 72 -> createFixed "Spell Scroll (Level 5)" Rare 640u
          | Range 73 75 -> createFixed "Elixir of Health" Rare 120u
          | Range 76 78 -> createFixed "Oil of Etherealness" Rare 1920u
          | Range 79 81 -> createFixed "Potion of Giant Strength (Fire Giant)" Rare 600u
          | Range 82 84 ->
              match roll 1 6 with
              | 1 -> createFixed "Quaal's Feather Token, Anchor" Rare 50u
              | 2 -> createFixed "Quaal's Feather Token, Bird" Rare 3000u
              | 3 -> createFixed "Quaal's Feather Token, Fan" Rare 250u
              | 4 -> createFixed "Quaal's Feather Token, Swan boat" Rare 3000u
              | 5 -> createFixed "Quaal's Feather Token, Tree" Rare 250u
              | _ -> createFixed "Quaal's Feather Token, Whip" Rare 250u
          | Range 85 87 -> createFixed "Scroll of Protection" Rare 180u
          | Range 88 89 -> createFixed "Bag of Beans" Rare 500u
          | Range 90 91 -> createFixed "Bead of Force" Rare 960u
          | 92 -> createFixed "Chime of Opening" Rare 1500u
          | 93 -> createFixed "Decanter of Endless Water" Uncommon 135000u
          | 94 -> createFixed "Eyes of Minute Seeing" Uncommon 2500u
          | 95 -> createFixed "Folding Boat" Rare 10000u
          | 96 -> createFixed "Heward's Handy Haversack" Rare 2000u
          | 97 -> createFixed "Horseshoes of Speed" Rare 5000u
          | 98 ->
              let beads = roll 1 6 + 3

              match beads with
              | 1 -> 300u
              | 2 -> 480u
              | 3 -> 960u
              | 4 -> 1600u
              | 5 -> 3840u
              | 6 -> 7680u
              | 7 -> 10400u
              | 8 -> 14000u
              | _ -> 17200u
              |> createFixed $"Necklace of Fireballs: {beads} beads" Rare
          | 99 -> createFixed "Periapt of Health" Uncommon 5000u
          | _ -> createFixed "Sending Stones" Uncommon 2000u ]

let D count random =
    let roll = roll random

    [ for _ in 1 .. roll 1 count ->
          match roll 1 100 with
          | Range 1 20 -> createFixed "Potion of Supreme Healing" VeryRare 2000u
          | Range 21 30 -> createFixed "Potion of Invisibility" VeryRare 180u
          | Range 31 40 -> createFixed "Potion of Speed" VeryRare 400u
          | Range 41 50 -> createFixed "Spell Scroll (Level 6)" VeryRare 1280u
          | Range 51 57 -> createFixed "Spell Scroll (Level 7)" VeryRare 2560u
          | Range 58 62 -> createFixed "Ammunition +3" VeryRare 400u
          | Range 63 67 -> createFixed "Oil of Sharpness" VeryRare 3200u
          | Range 68 72 -> createFixed "Potion of Flying" VeryRare 500u
          | Range 73 77 -> createFixed "Potion of Giant Strength (Cloud Giant)" VeryRare 800u
          | Range 78 82 -> createFixed "Potion of Longevity" VeryRare 9000u
          | Range 83 87 -> createFixed "Potion of Vitality" VeryRare 960u
          | Range 88 92 -> createFixed "Spell Scroll (Level 8)" VeryRare 5120u
          | Range 93 95 -> createFixed "Horseshoes of a Zephyr" VeryRare 1500u
          | Range 96 98 -> createFixed "Nolzur's Marvelous Pigments" VeryRare 200u
          | 99 -> createFixed "Bag Of Devouring" VeryRare 25000u
          | _ -> createFixed "Portable Hole" Rare 8000u ]

let E count random =
    let roll = roll random

    [ for _ in 1 .. roll 1 count ->
          match roll 1 100 with
          | Range 1 30 -> createFixed "Spell Scroll (Level 8)" VeryRare 5120u
          | Range 31 55 -> createFixed "Potion of Giant Strength (Storm Giant)" Legendary 1000u
          | Range 56 70 -> createFixed "Potion of Supreme Healing" VeryRare 2000u
          | Range 71 85 -> createFixed "Spell Scroll (Level 9)" Legendary 10240u
          | Range 86 93 -> createFixed "Universal Solvent" Legendary 300u
          | Range 94 98 -> createFixed "Arrow of Slaying" VeryRare 600u
          | _ -> createFixed "Sovereign Glue" Legendary 400u ]

let F count random =
    let roll = roll random

    [ for _ in 1 .. roll 1 count ->
          match roll 1 100 with
          | Range 1 15 -> createFixed "Weapon +1" Uncommon 1000u
          | Range 16 18 -> createFixed "Shield +1" Uncommon 1500u
          | Range 19 21 -> createFixed "Sentinel Shield" Uncommon 20000u
          | Range 22 23 -> createFixed "Amulet of Proof Against Detection and Location" Uncommon 20000u
          | Range 24 25 -> createFixed "Boots of Elvenkind" Uncommon 2500u
          | Range 26 27 -> createFixed "Boots of Striding and Springing" Uncommon 5000u
          | Range 28 29 -> createFixed "Bracers of Archery" Uncommon 1500u
          | Range 30 31 -> createFixed "Brooch of Shielding" Uncommon 7500u
          | Range 32 33 -> createFixed "Broom of Flying" Uncommon 8000u
          | Range 34 35 -> createFixed "Cloak of Elvenkind" Uncommon 5000u
          | Range 36 37 -> createFixed "Cloak of Protection" Uncommon 3500u
          | Range 38 39 -> createFixed "Gauntlets of Ogre Power" Uncommon 8000u
          | Range 40 41 -> createFixed "Hat of Disguise" Uncommon 5000u
          | Range 42 43 -> createFixed "Javelin of Lightning" Uncommon 1500u
          | Range 44 45 -> createFixed "Pearl of Power" Uncommon 6000u
          | Range 46 47 -> createFixed "Rod of the Pact Keeper +1" Uncommon 12000u
          | Range 48 49 -> createFixed "Slippers of Spider Climbing" Uncommon 5000u
          | Range 50 51 -> createFixed "Staff of the Adder" Uncommon 1800u
          | Range 52 53 -> createFixed "Staff of the Python" Uncommon 2000u
          | Range 54 55 -> createFixed "Sword of Vengeance" Uncommon 0u
          | Range 56 57 -> createFixed "Trident of Fish Command" Uncommon 800u
          | Range 58 59 -> createFixed "Wand of Magic Missiles" Uncommon 8000u
          | Range 60 61 -> createFixed "Wand of the War Mage +1" Uncommon 1200u
          | Range 62 63 -> createFixed "Wand of Web" Uncommon 8000u
          | Range 64 65 -> createFixed "Weapon of Warning" Uncommon 60000u
          | 66 -> createFixed "Adamantine armor (chain mail)" Uncommon 500u
          | 67 -> createFixed "Adamantine armor (chain shirt)" Uncommon 500u
          | 68 -> createFixed "Adamantine armor (scale mail)" Uncommon 500u
          | 69 -> createFixed "Bag of Tricks (Gray)" Uncommon 500u
          | 70 -> createFixed "Bag of Tricks (Rust)" Uncommon 500u
          | 71 -> createFixed "Bag of Tricks (Tan)" Uncommon 500u
          | 72 -> createFixed "Boots of the Winterlands" Uncommon 10000u
          | 73 -> createFixed "Circlet of Blasting" Uncommon 1500u
          | 74 -> createFixed "Deck of Illusions" Uncommon 6120u
          | 75 -> createFixed "Eversmoking Bottle" Uncommon 1000u
          | 76 -> createFixed "Eyes of Charming" Uncommon 3000u
          | 77 -> createFixed "Eyes of the Eagle" Uncommon 2500u
          | 78 -> createFixed "Figurine of Wondrous Power, Silver Raven" Uncommon 5000u
          | 79 -> createFixed "Gem of Brightness" Uncommon 5000u
          | 80 -> createFixed "Gloves of Missile Snaring" Uncommon 3000u
          | 81 -> createFixed "Gloves of Swimming and Climbing" Uncommon 2000u
          | 82 -> createFixed "Gloves of Thievery" Uncommon 5000u
          | 83 -> createFixed "Headband of Intellect" Uncommon 8000u
          | 84 -> createFixed "Helm of Telepathy" Uncommon 12000u
          | 85 -> createFixed "Instrument of the Bards, Doss Lute" Uncommon 28500u
          | 86 -> createFixed "Instrument of the Bards, Fochlucan Bandore" Uncommon 26500u
          | 87 -> createFixed "Instrument of the Bards, Mac-Fuirmidh Cittern" Uncommon 27000u
          | 88 -> createFixed "Medallion of Thoughts" Uncommon 3000u
          | 89 -> createFixed "Necklace of Adaptation" Uncommon 1500u
          | 90 -> createFixed "Periapt of Wound Closure" Uncommon 5000u
          | 91 -> createFixed "Pipes of Haunting" Uncommon 6000u
          | 92 -> createFixed "Pipes of the Sewers" Uncommon 2000u
          | 93 -> createFixed "Ring of Jumping" Uncommon 2500u
          | 94 -> createFixed "Ring of Mind Shielding" Uncommon 16000u
          | 95 -> createFixed "Ring of Warmth" Uncommon 1000u
          | 96 -> createFixed "Ring of Water Walking" Uncommon 1500u
          | 97 -> createFixed "Quiver of Ehlonna" Uncommon 1000u
          | 98 -> createFixed "Stone of Good Luck (Luckstone)" Uncommon 4200u
          | 99 -> createFixed "Wind Fan" Uncommon 1500u
          | _ -> createFixed "Winged Boots" Uncommon 8000u ]

let G count random =
    let roll = roll random

    [ for _ in 1 .. roll 1 count ->
          match roll 1 100 with
          | Range 01 11 -> createFixed "Weapon, +2" Rare 4000u
          | Range 12 14 ->
              match roll 1 8 with
              | 1 -> createFixed "Figurine of Wondrous Power, Bronze Griffon" Rare 8000u
              | 2 -> createFixed "Figurine of Wondrous Power, Ebony Fly" Rare 6000u
              | 3 -> createFixed "Figurine of Wondrous Power, Golden Lions" Rare 600u
              | 4 -> createFixed "Figurine of Wondrous Power, Ivory Goats" Rare 20000u
              | 5 -> createFixed "Figurine of Wondrous Power, Marble Elephant" Rare 6000u
              | 6
              | 7 -> createFixed "Figurine of Wondrous Power, Onyx Dog" Rare 3000u
              | _ -> createFixed "Figurine of Wondrous Power, Serpentine Owl" Rare 8000u
          | 15 -> createFixed "Adamantine armor (breastplate)" Uncommon 500u
          | 16 -> createFixed "Adamantine armor (splint)" Uncommon 500u
          | 17 -> createFixed "Amulet of Health" Rare 8000u
          | 18 -> createFixed "Armor of Vulnerability" Rare 500u
          | 19 -> createFixed "Arrow-Catching Shield" Rare 6000u
          | 20 -> createFixed "Belt of Dwarvenkind" Rare 6000u
          | 21 -> createFixed "Belt of Hill Giant Strength" Rare 8000u
          | 22 -> createFixed "Berserker Axe" Rare 500u
          | 23 -> createFixed "Boots of Levitation" Rare 4000u
          | 24 -> createFixed "Boots of Speed" Rare 4000u
          | 25 -> createFixed "Bowl of Commanding Water Elementals" Rare 8000u
          | 26 -> createFixed "Bracers of Defense" Rare 6000u
          | 27 -> createFixed "Brazier of Commanding Fire Elementals" Rare 8000u
          | 28 -> createFixed "Cape of the Mountebank" Rare 8000u
          | 29 -> createFixed "Censer of Controlling Air Elementals" Rare 8000u
          | 30 -> createFixed "Armor +1 (chain mail)" Rare 1500u
          | 31 -> createFixed "Armor of Resistance (chain mail)" Rare 6000u
          | 32 -> createFixed "Armor of Resistance (chain shirt)" Rare 6000u
          | 33 -> createFixed "Armor +1 (chain shirt)" Rare 1500u
          | 34 -> createFixed "Cloak of Displacement" Rare 60000u
          | 35 -> createFixed "Cloak of the Bat" Rare 6000u
          | 36 -> createFixed "Cube of Force" Rare 16000u
          | 37 -> createFixed "Daern's Instant Fortress" Rare 75000u
          | 38 -> createFixed "Dagger of Venom" Rare 2500u
          | 39 -> createFixed "Dimensional Shackles" Rare 3000u
          | 40 -> createFixed "Dragon Slayer" Rare 8000u
          | 41 -> createFixed "Elven Chain" Rare 4000u
          | 42 -> createFixed "Flame Tongue" Rare 5000u
          | 43 -> createFixed "Gem of Seeing" Rare 32000u
          | 44 -> createFixed "Giant Slayer" Rare 7000u
          | 45 -> createFixed "Glamoured Studded Leather" Rare 2000u
          | 46 -> createFixed "Helm of Teleportation" Rare 64000u
          | 47 -> createFixed "Horn of Blasting" Rare 450u
          | 48 ->
              match roll 1 100 with
              | Range 01 40 -> createFixed "Horn of Valhalla, Silver" Rare 5600u
              | Range 41 75 -> createFixed "Horn of Valhalla, Brass" Rare 8400u
              | Range 76 90 -> createFixed "Horn of Valhalla, Bronze" VeryRare 11200u
              | _ -> createFixed "Horn of Valhalla, Iron" Legendary 14000u
          | 49 -> createFixed "Instrument of the Bards (Canaith Mandolin)" Rare 30000u
          | 50 -> createFixed "Instrument of the Bards (Cii Lyre)" Rare 35000u
          | 51 -> createFixed "Ioun Stone (Awareness)" Rare 12000u
          | 52 -> createFixed "Ioun Stone (Protection)" Rare 1200u
          | 53 -> createFixed "Ioun Stone (Reserve)" Rare 6000u
          | 54 -> createFixed "Ioun Stone (Sustenance)" Rare 1000u
          | 55 -> createFixed "Iron Bands of Bilarro" Rare 4000u
          | 56 -> createFixed "Armor +1, leather" Rare 1500u
          | 57 -> createFixed "Armor of Resistance (leather)" Rare 6000u
          | 58 -> createFixed "Mace of Disruption" Rare 8000u
          | 59 -> createFixed "Mace of Smiting" Rare 7000u
          | 60 -> createFixed "Mace of Terror" Rare 8000u
          | 61 -> createFixed "Mantle of Spell Resistance" Rare 30000u
          | 62 -> createFixed "Necklace of Prayer Beads" Rare 5000u
          | 63 -> createFixed "Periapt of Proof Against Poison" Rare 5000u
          | 64 -> createFixed "Ring of Animal Influence" Rare 4000u
          | 65 -> createFixed "Ring of Evasion" Rare 5000u
          | 66 -> createFixed "Ring of Feather Falling" Rare 2000u
          | 67 -> createFixed "Ring of Free Action" Rare 20000u
          | 68 -> createFixed "Ring of Protection" Rare 3500u
          | 69 -> createFixed "Ring of Resistance" Rare 6000u
          | 70 -> createFixed "Ring of Spell Storing" Rare 24000u
          | 71 -> createFixed "Ring of the Ram" Rare 5000u
          | 72 -> createFixed "Ring of X-ray Vision" Rare 6000u
          | 73 -> createFixed "Robe of Eyes" Rare 30000u
          | 74 -> createFixed "Rod of Rulership" Rare 16000u
          | 75 -> createFixed "Rod of the Pact Keeper, +2" Rare 16000u
          | 76 -> createFixed "Rope of Entanglement" Rare 4000u
          | 77 -> createFixed "Armor +1 (scale mail)" Rare 1500u
          | 78 -> createFixed "Armor of Resistance (scale mail)" Rare 6000u
          | 79 -> createFixed "Shield +2" Rare 6000u
          | 80 -> createFixed "Shield of Missile Attraction" Rare 6000u
          | 81 -> createFixed "Staff of Charming" Rare 12000u
          | 82 -> createFixed "Staff of Healing" Rare 13000u
          | 83 -> createFixed "Staff of Swarming Insects" Rare 16000u
          | 84 -> createFixed "Staff of the Woodlands" Rare 44000u
          | 85 -> createFixed "Staff of Withering" Rare 3000u
          | 86 -> createFixed "Stone of Controlling Earth Elementals" Rare 8000u
          | 87 -> createFixed "Sun Blade" Rare 12000u
          | 88 -> createFixed "Sword of Life Stealing" Rare 1000u
          | 89 -> createFixed "Sword of Wounding" Rare 2000u
          | 90 -> createFixed "Tentacle Rod" Rare 5000u
          | 91 -> createFixed "Vicious Weapon" Rare 350u
          | 92 -> createFixed "Wand of Binding" Rare 10000u
          | 93 -> createFixed "Wand of Enemy Detection" Rare 4000u
          | 94 -> createFixed "Wand of Fear" Rare 10000u
          | 95 -> createFixed "Wand of Fireballs" Rare 32000u
          | 96 -> createFixed "Wand of Lightning Bolts" Rare 32000u
          | 97 -> createFixed "Wand of Paralysis" Rare 16000u
          | 98 -> createFixed "Wand of the War Mage +2" Rare 4800u
          | 99 -> createFixed "Wand of Wonder" Rare 2500u
          | _ -> createFixed "Wings of Flying" Rare 5000u ]

let H count random =
    let roll = roll random

    [ for _ in 1 .. roll 1 count ->
          match roll 1 100 with
          | Range 01 10 -> createFixed "Weapon +3" VeryRare 16000u
          | Range 11 12 -> createFixed "Amulet of the Planes" VeryRare 160000u
          | Range 13 14 -> createFixed "Carpet of Flying" VeryRare 12000u
          | Range 15 16 -> createFixed "Crystal Ball" VeryRare 50000u
          | Range 17 18 -> createFixed "Ring of Regeneration" VeryRare 12000u
          | Range 19 20 -> createFixed "Ring of Shooting Stars" VeryRare 14000u
          | Range 21 22 -> createFixed "Ring of Telekinesis" VeryRare 80000u
          | Range 23 24 -> createFixed "Robe of Scintillating Colors" VeryRare 6000u
          | Range 25 26 -> createFixed "Robe of Stars" VeryRare 60000u
          | Range 27 28 -> createFixed "Rod of Absorption" VeryRare 50000u
          | Range 29 30 -> createFixed "Rod of Alertness" VeryRare 25000u
          | Range 31 32 -> createFixed "Rod of Security" VeryRare 90000u
          | Range 33 34 -> createFixed "Rod of the Pact Keeper +3" VeryRare 28000u
          | Range 35 36 -> createFixed "Scimitar of Speed" VeryRare 6000u
          | Range 37 38 -> createFixed "Shield +3" VeryRare 24000u
          | Range 39 40 -> createFixed "Staff of Fire" VeryRare 16000u
          | Range 41 42 -> createFixed "Staff of Frost" VeryRare 26000u
          | Range 43 44 -> createFixed "Staff of Power" VeryRare 95500u
          | Range 45 46 -> createFixed "Staff of Striking" VeryRare 21000u
          | Range 47 48 -> createFixed "Staff of Thunder and Lightning" VeryRare 10000u
          | Range 49 50 -> createFixed "Sword of Sharpness" VeryRare 1700u
          | Range 51 52 -> createFixed "Wand of Polymorph" VeryRare 32000u
          | Range 53 54 -> createFixed "Wand of the War Mage +3" VeryRare 19200u
          | 55 -> createFixed "Adamantine armor (half plate)" Uncommon 500u
          | 56 -> createFixed "Adamantine armor (plate)" Uncommon 500u
          | 57 -> createFixed "Animated shield" VeryRare 6000u
          | 58 -> createFixed "Belt of Fire Giant Strength" VeryRare 75000u
          | 59 ->
              match roll 1 2 with
              | 1 -> createFixed "Belt of Frost Giant Strength" VeryRare 40000u
              | _ -> createFixed "Belt of Stone Giant Strength" VeryRare 40000u
          | 60 -> createFixed "Armor +1, breastplate" Rare 1500u
          | 61 -> createFixed "Armor of Resistance (breastplate)" Rare 6000u
          | 62 -> createFixed "Candle of Invocation" VeryRare 40000u
          | 63 -> createFixed "Armor +2, chain mail" VeryRare 6000u
          | 64 -> createFixed "Armor +2, chain shirt" VeryRare 6000u
          | 65 -> createFixed "Cloak of Arachnida" VeryRare 5000u
          | 66 -> createFixed "Dancing Sword" VeryRare 2000u
          | 67 -> createFixed "Demon Armor" VeryRare 5000u
          | 68 -> createFixed "Dragon Scale Mail" VeryRare 4000u
          | 69 -> createFixed "Dwarven Plate" VeryRare 9000u
          | 70 -> createFixed "Dwarven Thrower" VeryRare 18000u
          | 71 -> createFixed "Efreeti Bottle" VeryRare 50000u
          | 72 -> createFixed "Figurine of Wondrous Power (obsidian steed)" VeryRare 128000u
          | 73 -> createFixed "Frost Brand" VeryRare 2200u
          | 74 -> createFixed "Helm of Brilliance" VeryRare 25000u
          | 75 -> createFixed "Horn of Valhalla (bronze)" VeryRare 11200u
          | 76 -> createFixed "Instrument of the Bards (Anstruth Harp)" VeryRare 109000u
          | 77 -> createFixed "Ioun Stone (Absorption)" VeryRare 2400u
          | 78 -> createFixed "Ioun Stone (Agility)" VeryRare 3000u
          | 79 -> createFixed "Ioun Stone (Fortitude)" VeryRare 3000u
          | 80 -> createFixed "Ioun Stone (Insight)" VeryRare 3000u
          | 81 -> createFixed "Ioun Stone (Intellect)" VeryRare 3000u
          | 82 -> createFixed "Ioun Stone (Leadership)" VeryRare 3000u
          | 83 -> createFixed "Ioun Stone (Strength)" VeryRare 3000u
          | 84 -> createFixed "Armor +2 (leather)" VeryRare 6000u
          | 85 -> createFixed "Manual of Bodily Health" VeryRare 25000u
          | 86 -> createFixed "Manual of Gainful Exercise" VeryRare 25000u
          | 87 ->
              let material, value =
                  match roll 1 20 with
                  | Range 1 5 -> "Clay", 65000u
                  | Range 6 17 -> "Flesh", 50000u
                  | 18 -> "Iron", 100000u
                  | _ -> "Stone", 80000u

              createFixed $"Manual of Golems {material}" VeryRare value
          | 88 -> createFixed "Manual of Quickness of Action" VeryRare 25000u
          | 89 -> createFixed "Mirror of Life Trapping" VeryRare 18000u
          | 90 -> createFixed "Nine Lives Stealer" VeryRare 8000u
          | 91 -> createFixed "Oathbow" VeryRare 3500u
          | 92 -> createFixed "Armor +2 (scale mail)" VeryRare 6000u
          | 93 -> createFixed "Spellguard Shield" VeryRare 50000u
          | 94 -> createFixed "Armor +1 (splint)" Rare 1500u
          | 95 -> createFixed "Armor of Resistance (splint)" Rare 6000u
          | 96 -> createFixed "Armor +1 (studded leather)" Rare 1500u
          | 97 -> createFixed "Armor of Resistance (studded leather)" Rare 6000u
          | 98 -> createFixed "Tome of Clear Thought" VeryRare 25000u
          | 99 -> createFixed "Tome of Leadership and Influence" VeryRare 25000u
          | _ -> createFixed "Tome of Understanding" VeryRare 25000u ]

let I count random =
    let roll = roll random

    [ for _ in 1 .. roll 1 count ->
          match roll 1 100 with
          | Range 01 05 -> createFixed "Defender" Legendary 24000u
          | Range 06 10 -> createFixed "Hammer of Thunderbolts" Legendary 16000u
          | Range 11 15 -> createFixed "Luck Blade" Legendary 50000u
          | Range 16 20 -> createFixed "Sword of Answering" Legendary 36000u
          | Range 21 23 -> createFixed "Holy Avenger" Legendary 165000u
          | Range 24 26 -> createFixed "Ring of Djinni Summoning" Legendary 50000u
          | Range 27 29 -> createFixed "Ring of Invisibility" Legendary 10000u
          | Range 30 32 -> createFixed "Ring of Spell Turning" Legendary 30000u
          | Range 33 35 -> createFixed "Rod of Lordly Might" Legendary 28000u
          | Range 36 38 -> createFixed "Staff of the Magi" Legendary 50000u
          | Range 39 41 -> createFixed "Vorpal Sword" Legendary 24000u
          | Range 42 43 -> createFixed "Belt of Cloud Giant Strength" Legendary 120000u
          | Range 44 45 -> createFixed "Armor +2 (breastplate)" VeryRare 6000u
          | Range 46 47 -> createFixed "Armor +3 (chain mail)" Legendary 24000u
          | Range 48 49 -> createFixed "Armor +3 (chain shirt)" Legendary 24000u
          | Range 50 51 -> createFixed "Cloak of Invisibility" Legendary 80000u
          | Range 52 53 ->
              match roll 1 3 with
              | 1 -> createFixed "Crystal Ball of Mind Reading" Legendary 100_000u
              | 2 -> createFixed "Crystal Ball of Telepathy" Legendary 100_000u
              | _ -> createFixed "Crystal Ball of True Seeing" Legendary 100_000u
          | Range 54 55 -> createFixed "Armor +1 (half plate)" Rare 1500u
          | Range 56 57 -> createFixed "Iron Flask" Legendary 50000u
          | Range 58 59 -> createFixed "Armor +3 (leather)" Legendary 24000u
          | Range 60 61 -> createFixed "Armor +1 (plate)" Rare 1500u
          | Range 62 63 -> createFixed "Robe of the Archmagi" Legendary 34000u
          | Range 64 65 -> createFixed "Rod of Resurrection" Legendary 50000u
          | Range 66 67 -> createFixed "Armor +1, scale mail" Rare 1500u
          | Range 68 69 -> createFixed "Scarab of Protection" Legendary 36000u
          | Range 70 71 -> createFixed "Armor +2 (splint)" VeryRare 6000u
          | Range 72 73 -> createFixed "Armor +2 (studded leather)" VeryRare 6000u
          | Range 74 75 -> createFixed "Well of Many Worlds" Legendary 50000u
          | 76 ->
              match roll 1 12 with
              | Range 1 2 -> createFixed "Armor +2 (half plate)" VeryRare 6000u
              | Range 3 4 -> createFixed "Armor +2 (plate)" VeryRare 6000u
              | Range 5 6 -> createFixed "Armor +3 (studded leather)" Legendary 24000u
              | Range 7 8 -> createFixed "Armor +3 (breastplate)" Legendary 24000u
              | Range 9 10 -> createFixed "Armor +3 (splint)" Legendary 24000u
              | 11 -> createFixed "Armor +3 (half plate)" Legendary 24000u
              | _ -> createFixed "Armor +3 (plate)" Legendary 24000u
          | 77 -> createFixed "Apparatus of Kwalish" Legendary 10000u
          | 78 -> createFixed "Armor of Invulnerability" Legendary 18000u
          | 79 -> createFixed "Belt of Storm Giant Strength" Legendary 160000u
          | 80 -> createFixed "Cubic Gate" Legendary 40000u
          | 81 -> createFixed "Deck of Many Things" Legendary 100000u
          | 82 -> createFixed "Efreeti Chain" Legendary 20000u
          | 83 -> createFixed "Armor of Resistance (half plate)" Rare 6000u
          | 84 -> createFixed "Horn of Valhalla (iron)" Legendary 14000u
          | 85 -> createFixed "Instrument of the Bards (Ollamh Harp)" Legendary 125000u
          | 86 -> createFixed "Ioun Stone (Greater Absorption)" Legendary 31000u
          | 87 -> createFixed "Ioun Stone (Mastery)" Legendary 15000u
          | 88 -> createFixed "Ioun Stone (Regeneration)" Legendary 40000u
          | 89 -> createFixed "Plate Armor of Etherealness" Legendary 48000u
          | 90 -> createFixed "Armor of Resistance (plate)" Rare 6000u
          | 91 -> createFixed "Ring of Air Elemental Command" Legendary 35000u
          | 92 -> createFixed "Ring of Earth Elemental Command" Legendary 31000u
          | 93 -> createFixed "Ring of Fire Elemental Command" Legendary 17000u
          | 94 -> createFixed "Ring of Three Wishes" Legendary 150000u
          | 95 -> createFixed "Ring of Water Elemental Command" Legendary 25000u
          | 96 -> createFixed "Sphere of Annihilation" Legendary 15000u
          | 97 -> createFixed "Talisman of Pure Good" Legendary 71680u
          | 98 -> createFixed "Talisman of the Sphere" Legendary 20000u
          | 99 -> createFixed "Talisman of Ultimate Evil" Legendary 61440u
          | _ -> createFixed "Tome of the Stilled Tongue" Legendary 50000u ]
