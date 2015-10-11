nouns = ["hobo","frog","pope"]
adjectives = ["lazy","grouchy","scheming"]

main = print ([adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns])
