module SmallGame where
    data Player = Player {
        attack :: Int,
        defence :: Int,
        health :: Int,
        weapon :: Equipment,
        armor :: Equipment,
        buff :: Equipment
    }
    data Monster = Monster {
        mattack :: Int,
        mdefence :: Int,
        mhealth :: Int,
        loot :: [Equipment]
    }

    data Equipment = Armor Int | Weapon Int | Buff Int deriving (Eq, Ord)

    setEq :: Int -> Equipment -> Player -> Player
    setEq isTake (Armor x) p = p {defence = defence p + isTake * x}
    setEq isTake (Weapon x) p = p {attack = attack p + isTake * x}
    setEq isTake (Buff x) p = p {health = health p + isTake * x,
        defence = defence p + isTake * x, attack = attack p + isTake * x}


    dropEq :: Equipment -> Player -> Player
    dropEq e p = setEq (-1) e p

    equip :: Equipment -> Player -> Player
    equip e p = setEq 1 e p

    updateEq :: Equipment -> Player -> Player
    updateEq a@(Armor _) p = if a > armor p then equip a (dropEq (armor p) p) else p
    updateEq w@(Weapon _) p = if w > weapon p then equip w (dropEq (weapon p) p) else p
    updateEq b@(Buff _) p = if b > armor p then equip b (dropEq (buff p) p) else p

    armoredAttack :: Int -> Int -> Int -> Int
    armoredAttack att def h = if att > def then h - att + def else h

    playerHit :: Player -> Monster -> Monster
    playerHit p m = m {mhealth = armoredAttack (attack p) (mdefence m) (mhealth m)}

    monsterHit :: Player -> Monster -> Player
    monsterHit p m = p {health = armoredAttack (mattack m) (defence p) (health p)}



    upgradeEquipment :: [Equipment] -> Player -> Player
    upgradeEquipment [] p = p
    upgradeEquipment (x:xs) p = upgradeEquipment xs (updateEq x p)

    gloriousFight :: Player -> Monster -> Player
    gloriousFight p m = let m' = playerHit p m
        in if (mhealth m' < 0) then upgradeEquipment (loot m') p
            else let p' = monsterHit p m' in
                if (health p' > 0) then gloriousFight p' m'
                    else p


    gloriousBattle :: Player -> [Monster] -> String
    gloriousBattle p [] = "Victory!"
    gloriousBattle p (x:xs) = let p' = gloriousFight p x in
        if (health p' > 0) then gloriousBattle p' xs
            else "Player dead"


    