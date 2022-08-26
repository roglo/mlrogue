(* $Id: object.ml,v 1.55 2010/01/24 17:25:45 deraugla Exp $ *)

open Rogue;

value rand_percent = Imisc.rand_percent;
value get_rand = Imisc.get_rand;
value coin_toss = Imisc.coin_toss;

type interest = [ Useful | Neutral | Harmful ];

type object_desc 'a =
  { o_kind : 'a;
    o_title : string;
    o_interest : interest;
    o_value : int }
;

value int_of_object tab x =
  loop_i 0 where rec loop_i i =
    if i = Array.length tab then assert False
    else if x = tab.(i).o_kind then i
    else loop_i (i + 1)
;

(* armors *)

value armor_tab =
  [| {o_kind = Leather; o_title = "leather armor"; o_interest = Useful;
      o_value = 300};
     {o_kind = Ringmail; o_title = "ring mail"; o_interest = Useful;
      o_value = 300};
     {o_kind = Scale; o_title = "scale mail"; o_interest = Useful;
      o_value = 400};
     {o_kind = Chain; o_title = "chain mail"; o_interest = Useful;
      o_value = 500};
     {o_kind = Banded; o_title = "banded mail"; o_interest = Useful;
      o_value = 600};
     {o_kind = Splint; o_title = "splint mail"; o_interest = Useful;
      o_value = 600};
     {o_kind = Plate; o_title = "plate mail"; o_interest = Useful;
      o_value = 700} |]
;

value int_of_armor = int_of_object armor_tab;

(* potions *)

value potion_tab =
  [| {o_kind = IncreaseStrength; o_title = "of increase strength";
      o_interest = Useful; o_value = 100};
     {o_kind = RestoreStrength; o_title = "of restore strength";
      o_interest = Useful; o_value = 250};
     {o_kind = Healing; o_title = "of healing";
      o_interest = Useful; o_value = 100};
     {o_kind = ExtraHealing; o_title = "of extra healing";
      o_interest = Useful; o_value = 200};
     {o_kind = Poison; o_title = "of poison";
      o_interest = Harmful; o_value = 10};
     {o_kind = RaiseLevel; o_title = "of raise level";
      o_interest = Useful; o_value = 300};
     {o_kind = Blindness; o_title = "of blindness";
      o_interest = Harmful; o_value = 10};
     {o_kind = Hallucination; o_title = "of hallucination";
      o_interest = Harmful; o_value = 25};
     {o_kind = DetectMonsters; o_title = "of detect monster";
      o_interest = Useful; o_value = 100};
     {o_kind = DetectObjects; o_title = "of detect things";
      o_interest = Useful; o_value = 100};
     {o_kind = Confusion; o_title = "of confusion";
      o_interest = Harmful; o_value = 10};
     {o_kind = Levitation; o_title = "of levitation";
      o_interest = Harmful; o_value = 80};
     {o_kind = HasteSelf; o_title = "of haste self";
      o_interest = Useful; o_value = 150};
     {o_kind = SeeInvisible; o_title = "of see invisible";
      o_interest = Useful; o_value = 145} |]
;

value int_of_potion = int_of_object potion_tab;

(* rings *)

value ring_tab =
  [| {o_kind = Stealth; o_title = "of stealth";
      o_interest = Useful; o_value = 250};
     {o_kind = RTeleport; o_title = "of teleportation";
      o_interest = Neutral; o_value = 100};
     {o_kind = Regeneration; o_title = "of regeneration";
      o_interest = Useful; o_value = 255};
     {o_kind = SlowDigest; o_title = "of slow digestion";
      o_interest = Useful; o_value = 295};
     {o_kind = AddStrength; o_title = "of add strength";
      o_interest = Useful; o_value = 200};
     {o_kind = SustainStrength; o_title = "of sustain strength";
      o_interest = Useful; o_value = 250};
     {o_kind = Dexterity; o_title = "of dexterity";
      o_interest = Useful; o_value = 250};
     {o_kind = Adornment; o_title = "of adornment";
      o_interest = Neutral; o_value = 25};
     {o_kind = RSeeInvisible; o_title = "of see invisible";
      o_interest = Useful; o_value = 300};
     {o_kind = MaintainArmor; o_title = "of maintain armor";
      o_interest = Useful; o_value = 290};
     {o_kind = Searching; o_title = "of searching";
      o_interest = Useful; o_value = 270} |]
;

value int_of_ring = int_of_object ring_tab;

(* scrolls *)

value scroll_tab =
  [| {o_kind = ProtectArmor; o_title = "of protect armor";
      o_interest = Useful; o_value = 505};
     {o_kind = HoldMonster; o_title = "of hold monster";
      o_interest = Useful; o_value = 200};
     {o_kind = EnchantWeapon; o_title = "of enchant weapon";
      o_interest = Useful; o_value = 235};
     {o_kind = EnchantArmor; o_title = "of enchant armor";
      o_interest = Useful; o_value = 235};
     {o_kind = Identify; o_title = "of identify";
      o_interest = Useful; o_value = 175};
     {o_kind = Teleport; o_title = "of teleportation";
      o_interest = Useful; o_value = 190};
     {o_kind = Sleep; o_title = "of sleep";
      o_interest = Harmful; o_value = 25};
     {o_kind = ScareMonster; o_title = "of scare monster";
      o_interest = Useful; o_value = 610};
     {o_kind = RemoveCurse; o_title = "of remove curse";
      o_interest = Useful; o_value = 210};
     {o_kind = CreateMonster; o_title = "of create monster";
      o_interest = Harmful; o_value = 100};
     {o_kind = AggravateMonster; o_title = "of aggravate monster";
      o_interest = Harmful; o_value = 25};
     {o_kind = MagicMapping; o_title = "of magic mapping";
      o_interest = Useful; o_value = 180} |]
;

value int_of_scroll = int_of_object scroll_tab;

(* wands *)

value wand_tab =
  [| {o_kind = TeleportAway; o_title = "of teleport away";
      o_interest = Useful; o_value = 25};
     {o_kind = SlowMonster; o_title = "of slow monster";
      o_interest = Useful; o_value = 50};
     {o_kind = ConfuseMonster; o_title = "of confuse monster";
      o_interest = Useful; o_value = 45};
     {o_kind = Invisibility; o_title = "of invisibility";
      o_interest = Harmful; o_value = 8};
     {o_kind = Polymorph; o_title = "of polymorph";
      o_interest = Neutral; o_value = 55};
     {o_kind = HasteMonster; o_title = "of haste monster";
      o_interest = Harmful; o_value = 2};
     {o_kind = PutToSleep; o_title = "of sleep";
      o_interest = Useful; o_value = 25};
     {o_kind = MagicMissile; o_title = "of magic missile";
      o_interest = Useful; o_value = 20};
     {o_kind = Cancellation; o_title = "of cancellation";
      o_interest = Useful; o_value = 20};
     {o_kind = DoNothing; o_title = "of do nothing";
      o_interest = Neutral; o_value = 0} |]
;

value int_of_wand = int_of_object wand_tab;

(* weapons *)

value weapon_tab =
  [| {o_kind = Bow; o_title = "short bow";
      o_interest = Useful; o_value = 150};
     {o_kind = Dart; o_title = "dart/darts";
      o_interest = Useful; o_value = 8};
     {o_kind = Arrow; o_title = "arrow/arrows";
      o_interest = Useful; o_value = 15};
     {o_kind = Dagger; o_title = "dagger/daggers";
      o_interest = Useful; o_value = 27};
     {o_kind = Shuriken; o_title = "shuriken/shurikens";
      o_interest = Useful; o_value = 35};
     {o_kind = Mace; o_title = "mace";
      o_interest = Useful; o_value = 360};
     {o_kind = LongSword; o_title = "long sword";
      o_interest = Useful; o_value = 470};
     {o_kind = TwoHandedSword; o_title = "two-handed sword";
      o_interest = Useful; o_value = 580} |]
;

value int_of_weapon = int_of_object weapon_tab;

(* *)

value colours =
  [| "blue"; "red"; "green"; "grey"; "brown"; "clear"; "pink"; "white";
     "purple"; "black"; "yellow"; "plaid"; "burgundy"; "beige" |]
;

(* *)

value create_obj kind q =
  {ob_kind = kind; ob_quantity = q; ob_row = 0; ob_col = 0;
   ob_picked_up = False}
;

value default_fruit = "slime-mold";

value get_amulet _ =
  create_obj Amulet 1
;

value get_food f =
  let f =
    match f with
    [ Some f -> f
    | None -> if rand_percent 80 then Ration else Fruit ]
  in
  create_obj (Food f) 1
;

value get_gold q =
  let q =
    match q with
    [ Some q -> q
    | None -> 0 ]
  in
  create_obj Gold q
;

value gr_potion p =
  let p =
    match p with
    [ Some p -> p
    | None ->
        let percent = get_rand 1 118 in
        if percent <= 5 then RaiseLevel
        else if percent <= 15 then DetectObjects
        else if percent <= 25 then DetectMonsters
        else if percent <= 35 then IncreaseStrength
        else if percent <= 45 then RestoreStrength
        else if percent <= 55 then Healing
        else if percent <= 65 then ExtraHealing
        else if percent <= 75 then Blindness
        else if percent <= 85 then Hallucination
        else if percent <= 95 then Confusion
        else if percent <= 105 then Poison
        else if percent <= 110 then Levitation
        else if percent <= 114 then HasteSelf
        else SeeInvisible ]
  in
  create_obj (Potion p) 1
;

value gr_scroll s =
  let s =
    match s with
    [ Some s -> s
    | None ->
        let percent = get_rand 0 85 in
        if percent <= 5 then ProtectArmor
        else if percent <= 11 then HoldMonster
        else if percent <= 20 then CreateMonster
        else if percent <= 35 then Identify
        else if percent <= 43 then Teleport
        else if percent <= 50 then Sleep
        else if percent <= 55 then ScareMonster
        else if percent <= 64 then RemoveCurse
        else if percent <= 69 then EnchantArmor
        else if percent <= 74 then EnchantWeapon
        else if percent <= 80 then AggravateMonster
        else MagicMapping ]
  in
  create_obj (Scroll s) 1
;

value gr_wand wk =
  let wk =
    match wk with
    [ Some wk -> wk
    | None ->
        let k = get_rand 0 (Array.length wand_tab - 1) in
        wand_tab.(k).o_kind ]
  in
  let h =
    match wk with
    [ TeleportAway | SlowMonster | ConfuseMonster | Invisibility
    | Polymorph | HasteMonster | PutToSleep | DoNothing -> get_rand 3 6
    | MagicMissile -> get_rand 6 12
    | Cancellation -> get_rand 5 9 ]
  in
  let w = {wa_kind = wk; wa_hits = h; wa_identified = False} in
  create_obj (Wand w) 1
;

value set_weapon_damage =
  fun
  [ Bow | Dart -> (1, 1, None)
  | Arrow -> (1, 2, None)
  | Dagger -> (1, 3, None)
  | Shuriken -> (1, 4, None)
  | Mace -> (2, 3, None)
  | LongSword -> (3, 4, None)
  | TwoHandedSword -> (4, 5, None) ]
;

value gr_weapon wkind =
  let w =
    let wk =
      match wkind with
      [ Some wk -> wk
      | None ->
          let k = get_rand 0 (Array.length weapon_tab - 1) in
          weapon_tab.(k).o_kind ]
    in
    let quiver =
      match wk with
      [ Arrow | Dagger | Shuriken | Dart -> get_rand 1 126
      | _ -> 0 ]
    in
    let percent = get_rand 1 96 in
    let blessing = get_rand 1 3 in
    let (he, de, is_cursed) =
      if percent <= 32 then
        let increment = if percent <= 16 then 1 else -1 in
        loop_i 0 0 0 where rec loop_i i he de =
          if i = blessing then (he, de, increment < 0)
          else if coin_toss () then loop_i (i + 1) (he + increment) de
          else loop_i (i + 1) he (de + increment)
      else (0, 0, False)
    in
    let dm = set_weapon_damage wk in
    {we_kind = wk; we_damage = dm; we_quiver = quiver;
     we_is_cursed = is_cursed; we_has_been_uncursed = False;
     we_hit_enchant = he; we_d_enchant = de; we_in_use = False;
     we_identified = False}
  in
  let q =
    match w.we_kind with
    [ Arrow | Dagger | Shuriken | Dart -> get_rand 3 15
    | _ -> 1 ]
  in
  create_obj (Weapon w) q
;

value gr_armor ak =
  let ak =
    match ak with
    [ Some ak -> ak
    | None ->
        let k = get_rand 0 (Array.length armor_tab - 1) in
        armor_tab.(k).o_kind ]
  in
  let ac =
    match ak with
    [ Leather -> 2
    | Ringmail -> 3
    | Scale -> 4
    | Chain -> 5
    | Banded -> 6
    | Splint -> 6
    | Plate -> 7 ]
  in
  let (ae, is_cursed) =
    let percent = get_rand 1 100 in
    let blessing = get_rand 1 3 in
    if percent <= 16 then (-blessing, True)
    else if percent <= 33 then (blessing, False)
    else (0, False)
  in 
  let a =
    {ar_kind = ak; ar_class = ac; ar_is_cursed = is_cursed;
     ar_is_protected = False; ar_enchant = ae; ar_in_use = False;
     ar_identified = False}
  in
  create_obj (Armor a) 1
;

value gr_ring rkind =
  let r =
    let rk =
      match rkind with
      [ Some rk -> rk
      | None ->
          let k = get_rand 0 (Array.length ring_tab - 1) in
          ring_tab.(k).o_kind ]
    in
    let (rc, is_cursed) =
      match rk with
      [ RTeleport -> (0, True)
      | AddStrength | Dexterity ->
          let rc =
            loop () where rec loop () =
              let rc = get_rand 0 4 - 2 in
              if rc = 0 then loop () else rc
          in
          (rc, rc < 0)
      | Adornment -> (0, coin_toss ())
      | Stealth | Regeneration | SlowDigest | SustainStrength | RSeeInvisible
      | MaintainArmor | Searching -> (0, False) ]
    in
    {rg_kind = rk; rg_class = rc; rg_is_cursed = is_cursed; rg_in_use = None;
     rg_identified = False}
  in
  create_obj (Ring r) 1
;

value gr_object g =
  if g.foods < g.cur_level / 2 then do {
    g.foods := g.foods + 1;
    get_food None
  }
  else
    let percent = get_rand 1 91 in
    if percent <= 30 then gr_scroll None
    else if percent <= 60 then gr_potion None
    else if percent <= 64 then gr_wand None
    else if percent <= 74 then gr_weapon None
    else if percent <= 83 then gr_armor None
    else if percent <= 88 then get_food None
    else gr_ring None
;
