﻿namespace InfinityRpg
open Prime

type [<ReferenceEquality; NoComparison>] Inventory =
    { Items : Map<ItemType, int>
      Gold : int }

    static member initial =
        { Items = Map.empty; Gold = 0 }

type [<ReferenceEquality; NoComparison>] Character =
    { CharacterIndex : CharacterIndex
      CharacterType : CharacterType
      FacingDirection : Direction
      ExpPoints : int
      HitPoints : int
      SpecialPoints : int
      PowerBuff : single
      ShieldBuff : single
      MagicBuff : single
      CounterBuff : single
      Statuses : StatusType Set
      WeaponOpt : WeaponType option
      ArmorOpt : ArmorType option
      Accessories : AccessoryType list }

    member this.IsAlly =
        match this.CharacterType with Ally _ -> true | Enemy _ -> false

    member this.IsEnemy =
        not this.IsAlly

    member this.IsAlive =
        this.HitPoints > 0
    
    member this.HitPointsMax =
        match this.CharacterType with Ally _ -> 30 | Enemy _ -> 10
    
    static member updateFacingDirection updater character =
        { character with FacingDirection = updater character.FacingDirection }
    
    static member updateHitPoints updater character =
        { character with HitPoints = updater character.HitPoints |> max 0 |> min character.HitPointsMax }

    static member empty =
        { CharacterIndex = PlayerIndex
          CharacterType = Ally Player
          FacingDirection = Upward
          ExpPoints = 0
          HitPoints = 10 // note this is an arbitrary number as hp max is calculated
          SpecialPoints = 1 // sp max is calculated
          PowerBuff = 1.0f // rate at which power is buffed / debuffed
          MagicBuff = 1.0f // rate at which magic is buffed / debuffed
          ShieldBuff = 1.0f // rate at which shield is buffed / debuffed
          CounterBuff = 1.0f // rate at which counter is buffed / debuffed
          Statuses = Set.empty<StatusType>
          WeaponOpt = Option<WeaponType>.None
          ArmorOpt = Option<ArmorType>.None
          Accessories = [] } // level is calculated from base experience + added experience

    static member makePlayer =
        Character.updateHitPoints (constant Character.empty.HitPointsMax) Character.empty

    static member makeEnemy index =
        let character = { Character.empty with CharacterIndex = index; CharacterType = Enemy Goopy }
        Character.updateHitPoints (constant character.HitPointsMax) character

type CharacterAnimationType =
    | CharacterAnimationFacing
    | CharacterAnimationActing
    | CharacterAnimationDefending
    | CharacterAnimationSpecial // works for jump, cast magic, being healed, and perhaps others!
    | CharacterAnimationSlain

type [<ReferenceEquality; NoComparison>] CharacterAnimationState =
    { StartTime : int64
      AnimationType : CharacterAnimationType
      Direction : Direction }
    
    static member initial =
        { StartTime = 0L
          AnimationType = CharacterAnimationFacing
          Direction = Upward }

    static member make time animationType direction =
        { StartTime = time
          AnimationType = animationType
          Direction = direction }