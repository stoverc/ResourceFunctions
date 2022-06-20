(* ::Package:: *)

(* ::Title:: *)
(*PlayWordle*)


(* ::Text:: *)
(*Play the popular online word game Wordle*)


(* ::Section:: *)
(*Main definition*)


ClearAll[style];

style[y_,color_]:=Highlighted[Style[y,FontSize->28,FontFamily->"Arial",White,FontWeight->Bold]/.{Highlighted[a_,opts___]:>a},Background->color,ImageSize->{40,40},Alignment->{Center,Center},BaselinePosition->Bottom];


ClearAll[PlayWordle]

PlayWordle[]:=DynamicModule[{
	guessnumber=0,
	(* list=Select[WordList[],StringLength[#]==5&], *)
	(* list=ResourceData[ResourceObject["Wordle Word List"]], *)
	list=ToLowerCase[{"about", "above", "acids", "acres", "acted", "actor", "acute", "adapt", "added", "admit", "adult", "after", "again", "agent", "aging", "agree", "ahead", "aimed", "alarm", "alert", "alien", "alike", "alive", "allow", "alone", "along", "aloud", "among", "amuse", "angel", "anger", "angle", "angry", "ankle", "annoy", "apart", "apple", "apply", "April", "apron", "Arabs", "areas", "arent", "argue", "arise", "armed", "armor", "arose", "array", "arrow", "ashes", "aside", "asked", "atlas", "atoms", "attic", "aunts", "avoid", "awake", "award", "aware", "awful", "awoke", "backs", "bacon", "badge", "badly", "baked", "bakes", "balls", "bands", "bangs", "banks", "barks", "barns", "based", "bases", "basic", "basin", "basis", "beach", "beads", "beaks", "beams", "beans", "beard", "bears", "beast", "beats", "began", "begin", "begun", "being", "bells", "belly", "below", "belts", "bench", "bends", "berry", "bible", "bikes", "bills", "birds", "birth", "bites", "black", "blade", "blame", "blank", "blast", "blaze", "bleed", "blend", "blind", "block", "blood", "bloom", "blown", "blows", "blues", "blunt", "blush", "board", "boats", "boils", "bombs", "bones", "bonus", "books", "boots", "bored", "bound", "bowed", "bowls", "boxer", "boxes", "brain", "brake", "brand", "brass", "brave", "bread", "break", "brick", "bride", "brief", "bring", "broad", "broke", "brook", "broom", "brown", "brush", "build", "built", "bulbs", "bulls", "bumps", "bumpy", "bunch", "bunks", "bunny", "burns", "burnt", "burst", "buses", "cabin", "cable", "caged", "cages", "cakes", "calls", "camel", "camps", "canal", "canoe", "cards", "cared", "cares", "cargo", "carry", "carts", "cases", "catch", "cause", "caves", "cells", "chain", "chair", "chalk", "charm", "chart", "chase", "cheap", "check", "cheek", "cheer", "chest", "chief", "child", "chill", "china", "chips", "choir", "chops", "chord", "chose", "cigar", "civil", "claim", "clams", "clash", "class", "claws", "clean", "clear", "clerk", "click", "cliff", "climb", "cling", "cloak", "clock", "close", "cloth", "cloud", "clown", "clubs", "clues", "clump", "clung", "coach", "coals", "coast", "coats", "cocoa", "coins", "color", "comes", "comic", "comma", "cones", "cooks", "cools", "coral", "cords", "costs", "cough", "could", "count", "court", "cover", "crabs", "crack", "craft", "crane", "crash", "crawl", "crazy", "cream", "creek", "creep", "crept", "crest", "crews", "cried", "cries", "crime", "crisp", "crops", "cross", "crowd", "crown", "crude", "cruel", "crush", "crust", "cubes", "cubic", "curls", "curly", "curve", "cycle", "daddy", "daily", "dairy", "daisy", "dance", "dared", "dares", "dates", "deals", "death", "debts", "decay", "delay", "dense", "depth", "desks", "devil", "diary", "didnt", "dimly", "dirty", "disco", "disks", "ditch", "dived", "diver", "docks", "doing", "dolls", "dolly", "domes", "doors", "doubt", "dough", "dozen", "draft", "drain", "drama", "drank", "drawn", "draws", "dread", "dream", "dress", "dried", "drier", "drift", "drill", "drily", "drink", "drive", "drops", "drove", "drown", "drugs", "drums", "drunk", "ducks", "dunes", "dusty", "dutch", "dying", "eager", "eagle", "early", "earth", "easel", "eaten", "edged", "edges", "Egypt", "eight", "elbow", "elder", "elves", "empty", "ended", "enemy", "enjoy", "enter", "entry", "equal", "erase", "error", "essay", "event", "every", "exact", "exist", "exits", "extra", "fable", "faced", "faces", "facts", "faded", "fails", "faint", "fairy", "faith", "falls", "false", "fancy", "farms", "fatal", "fatty", "fault", "favor", "fears", "feast", "feeds", "feels", "fence", "ferns", "ferry", "fetch", "fever", "fewer", "field", "fiery", "fifth", "fifty", "fight", "fills", "films", "final", "finds", "finer", "fired", "fires", "first", "fists", "fixed", "flags", "flame", "flaps", "flash", "flask", "flats", "fleet", "flesh", "flies", "flint", "float", "flock", "flood", "floor", "flour", "flown", "flows", "fluff", "fluid", "flung", "flute", "focus", "foggy", "folds", "folks", "foods", "force", "forms", "forth", "forts", "forty", "found", "fours", "foxes", "frail", "frame", "freed", "fresh", "fried", "fries", "frogs", "front", "frost", "frown", "fruit", "fuels", "fully", "fumes", "funds", "funny", "furry", "gains", "gales", "games", "gases", "gates", "gauge", "gazed", "gears", "geese", "germs", "ghost", "giant", "giddy", "gifts", "girls", "given", "gives", "glare", "glass", "glide", "globe", "glory", "glove", "glued", "glues", "goals", "goats", "going", "goods", "goose", "grabs", "grace", "grade", "grain", "grand", "grant", "graph", "grasp", "grass", "grave", "graze", "great", "Greek", "green", "greys", "grief", "grind", "grins", "group", "grove", "growl", "grown", "grows", "guard", "guess", "guest", "guide", "gulls", "habit", "hadnt", "hairs", "halls", "hands", "handy", "hangs", "happy", "harms", "harsh", "hasnt", "haste", "hatch", "hated", "hates", "haunt", "heads", "heals", "heard", "hears", "heart", "heats", "heavy", "heels", "hello", "helps", "hence", "herbs", "herds", "heres", "hides", "hills", "hilly", "hired", "hobby", "holds", "holes", "homes", "honey", "Honor", "hooks", "hoped", "hopes", "horns", "horse", "hotel", "hours", "house", "human", "humor", "hunts", "hurry", "hurts", "hutch", "ideal", "ideas", "image", "index", "India", "inner", "Irish", "irons", "issue", "Italy", "items", "japan", "jeans", "jello", "Jesus", "jewel", "joins", "joint", "jokes", "jolly", "judge", "juice", "juicy", "jumps", "keeps", "kicks", "kills", "kinds", "kings", "kites", "knees", "knelt", "knife", "knits", "knobs", "knock", "knots", "known", "knows", "label", "labor", "lakes", "lambs", "lamps", "lands", "lanes", "large", "lasts", "later", "Latin", "laugh", "lawns", "layer", "leads", "leaps", "learn", "least", "leave", "ledge", "legal", "lemon", "lends", "level", "lever", "lifts", "light", "liked", "likes", "limbs", "limit", "lined", "linen", "lines", "links", "lions", "lists", "liter", "lived", "liver", "lives", "loads", "local", "locks", "lodge", "lofty", "looks", "loops", "loose", "lords", "loser", "loses", "loved", "lover", "loves", "lower", "loyal", "lucky", "lumps", "lunar", "lunch", "lungs", "lying", "magic", "maids", "mails", "major", "maker", "makes", "males", "maple", "march", "marks", "marry", "marsh", "masks", "match", "maybe", "mayor", "meals", "means", "meats", "medal", "media", "meets", "melts", "mends", "mercy", "merry", "metal", "meter", "metre", "midst", "might", "miles", "milky", "mills", "mimes", "minds", "mined", "miner", "minor", "minus", "mists", "mixed", "mixes", "model", "moist", "molds", "moles", "money", "month", "moods", "moons", "moose", "moral", "moths", "motor", "mound", "mount", "mouse", "mouth", "moved", "moves", "movie", "muddy", "mules", "mummy", "music", "myths", "nails", "naked", "named", "names", "nanny", "nasty", "naval", "necks", "needs", "nerve", "nests", "never", "newer", "newly", "nicer", "night", "nines", "ninth", "noble", "noise", "noisy", "noone", "north", "noses", "nosey", "noted", "notes", "nouns", "novel", "nurse", "nylon", "oasis", "obeys", "occur", "ocean", "oddly", "offer", "often", "oiled", "older", "olive", "onion", "opens", "opera", "orbit", "order", "organ", "other", "ought", "ounce", "outer", "ovens", "owned", "owner", "packs", "pages", "pains", "paint", "pairs", "palms", "panel", "panic", "pants", "paper", "parks", "parts", "party", "paste", "patch", "paths", "pause", "paved", "peace", "peach", "peaks", "pearl", "pears", "pedal", "peels", "peeps", "pence", "penny", "perch", "phase", "phone", "photo", "piano", "picks", "piece", "piled", "piles", "pills", "pilot", "pinch", "pines", "pipes", "pitch", "place", "plain", "plait", "plane", "plank", "plans", "plant", "plate", "plays", "plows", "pluck", "plump", "poems", "poets", "point", "poked", "polar", "poles", "ponds", "pools", "porch", "ports", "posts", "pound", "pours", "power", "press", "price", "pride", "prime", "print", "prism", "prize", "proof", "proud", "prove", "puffs", "pulls", "pulse", "pumps", "punch", "pupil", "puppy", "purse", "pussy", "quack", "queen", "queer", "queue", "quick", "quiet", "quilt", "quite", "raced", "races", "racks", "radar", "radio", "rafts", "rails", "rains", "rainy", "raise", "rally", "ranch", "range", "ranks", "rapid", "rates", "ratio", "razor", "reach", "react", "reads", "ready", "reeds", "refer", "reign", "reins", "relax", "relay", "reply", "rests", "rhyme", "rider", "rides", "ridge", "rifle", "right", "rigid", "rings", "rinks", "riots", "ripen", "rises", "rival", "river", "roads", "roars", "roast", "robes", "robin", "robot", "rocks", "rocky", "rolls", "roman", "roofs", "rooms", "roots", "roped", "ropes", "roses", "rough", "round", "route", "royal", "ruins", "ruled", "ruler", "rules", "rungs", "rural", "rusty", "sacks", "sadly", "safer", "sails", "salad", "sales", "salty", "sands", "sandy", "sauce", "saved", "saves", "scale", "scare", "scarf", "scary", "scene", "scent", "score", "scout", "scrap", "screw", "scrub", "seals", "seats", "seeds", "seems", "seize", "sells", "sends", "sense", "serve", "seven", "sewed", "shade", "shady", "shaft", "shake", "shall", "shame", "shape", "share", "shark", "sharp", "sheds", "sheep", "sheer", "sheet", "shelf", "shell", "shift", "shine", "shiny", "ships", "shirt", "shock", "shoes", "shone", "shook", "shoot", "shops", "short", "shots", "shout", "shown", "shows", "shuts", "sides", "sight", "signs", "silks", "silky", "silly", "since", "sings", "sinks", "sirup", "sixes", "sixth", "sixty", "sized", "sizes", "skate", "skied", "skies", "skill", "skins", "skips", "skirt", "skull", "skunk", "slant", "slave", "sleek", "sleep", "slept", "slice", "slide", "slips", "slope", "slows", "smack", "small", "smart", "smash", "smell", "smelt", "smile", "smoke", "snack", "snail", "snake", "snaps", "sneak", "snows", "snowy", "socks", "soils", "solar", "solid", "solve", "songs", "sorry", "sorts", "sound", "south", "space", "spade", "Spain", "spare", "spark", "speak", "spear", "speed", "spell", "spelt", "spend", "spent", "sperm", "spill", "spine", "spins", "spite", "split", "spoil", "spoke", "spoon", "sport", "spots", "spray", "stack", "staff", "stage", "stair", "stalk", "stall", "stamp", "stand", "stare", "stars", "start", "state", "stays", "steak", "steal", "steam", "steel", "steep", "steer", "stems", "steps", "stern", "stick", "stiff", "still", "sting", "stirs", "stock", "stole", "stone", "stony", "stood", "stool", "stoop", "stops", "store", "storm", "story", "stout", "stove", "strap", "straw", "stray", "strip", "stuck", "study", "stuff", "stump", "stung", "stuns", "stunt", "style", "sugar", "suits", "sunny", "super", "swamp", "swear", "sweep", "sweet", "swell", "swept", "swift", "swims", "swing", "Swiss", "sword", "swung", "table", "tails", "taken", "takes", "tales", "talks", "tanks", "tarts", "tasks", "taste", "tasty", "taxes", "teach", "teams", "tears", "tease", "teddy", "teeth", "tells", "tempo", "tends", "tense", "tenth", "tents", "terms", "tests", "thank", "their", "theme", "there", "these", "theyd", "thick", "thief", "thing", "think", "third", "thorn", "those", "three", "threw", "throw", "thumb", "thump", "tidal", "tides", "tiger", "tight", "tiles", "timer", "times", "timid", "tired", "tires", "title", "toads", "toast", "today", "tones", "tools", "tooth", "topic", "torch", "total", "touch", "tough", "towel", "tower", "towns", "trace", "track", "trade", "trail", "train", "tramp", "traps", "treat", "trees", "trend", "trial", "tribe", "trick", "tried", "tries", "trips", "troop", "trout", "truck", "truly", "trunk", "trust", "truth", "tubes", "tummy", "tunes", "turns", "tusks", "twice", "twigs", "twins", "twist", "tying", "types", "tyres", "uncle", "under", "union", "units", "unity", "untie", "until", "upper", "upset", "urban", "urged", "using", "usual", "utter", "vague", "value", "valve", "Vapor", "vases", "veins", "venus", "verbs", "verse", "video", "views", "vines", "virus", "visit", "vital", "vivid", "vocal", "voice", "voted", "votes", "vowel", "waded", "wages", "wagon", "waist", "waits", "wakes", "walks", "walls", "wants", "warms", "wasnt", "waste", "watch", "water", "waved", "waves", "wears", "weary", "weave", "weeds", "weeks", "weigh", "weird", "wells", "whale", "wheat", "wheel", "where", "which", "while", "white", "whole", "whose", "wider", "widow", "width", "winds", "windy", "wings", "wiped", "wipes", "wired", "wires", "witch", "wives", "woman", "women", "woods", "words", "works", "world", "worms", "worry", "worse", "worst", "worth", "would", "wound", "woven", "wraps", "wreck", "wrist", "write", "wrong", "wrote", "xrays", "yacht", "yards", "yawns", "years", "yield", "yolks", "youll", "young", "youre", "yours", "youth", "youve", "yoyos", "zebra"}],
	list2=ToLowerCase[Union[{"about", "above", "acids", "acres", "acted", "actor", "acute", "adapt", "added", "admit", "adult", "after", "again", "agent", "aging", "agree", "ahead", "aimed", "alarm", "alert", "alien", "alike", "alive", "allow", "alone", "along", "aloud", "among", "amuse", "angel", "anger", "angle", "angry", "ankle", "annoy", "apart", "apple", "apply", "April", "apron", "Arabs", "areas", "arent", "argue", "arise", "armed", "armor", "arose", "array", "arrow", "ashes", "aside", "asked", "atlas", "atoms", "attic", "aunts", "avoid", "awake", "award", "aware", "awful", "awoke", "backs", "bacon", "badge", "badly", "baked", "bakes", "balls", "bands", "bangs", "banks", "barks", "barns", "based", "bases", "basic", "basin", "basis", "beach", "beads", "beaks", "beams", "beans", "beard", "bears", "beast", "beats", "began", "begin", "begun", "being", "bells", "belly", "below", "belts", "bench", "bends", "berry", "bible", "bikes", "bills", "birds", "birth", "bites", "black", "blade", "blame", "blank", "blast", "blaze", "bleed", "blend", "blind", "block", "blood", "bloom", "blown", "blows", "blues", "blunt", "blush", "board", "boats", "boils", "bombs", "bones", "bonus", "books", "boots", "bored", "bound", "bowed", "bowls", "boxer", "boxes", "brain", "brake", "brand", "brass", "brave", "bread", "break", "brick", "bride", "brief", "bring", "broad", "broke", "brook", "broom", "brown", "brush", "build", "built", "bulbs", "bulls", "bumps", "bumpy", "bunch", "bunks", "bunny", "burns", "burnt", "burst", "buses", "cabin", "cable", "caged", "cages", "cakes", "calls", "camel", "camps", "canal", "canoe", "cards", "cared", "cares", "cargo", "carry", "carts", "cases", "catch", "cause", "caves", "cells", "chain", "chair", "chalk", "charm", "chart", "chase", "cheap", "check", "cheek", "cheer", "chest", "chief", "child", "chill", "china", "chips", "choir", "chops", "chord", "chose", "cigar", "civil", "claim", "clams", "clash", "class", "claws", "clean", "clear", "clerk", "click", "cliff", "climb", "cling", "cloak", "clock", "close", "cloth", "cloud", "clown", "clubs", "clues", "clump", "clung", "coach", "coals", "coast", "coats", "cocoa", "coins", "color", "comes", "comic", "comma", "cones", "cooks", "cools", "coral", "cords", "costs", "cough", "could", "count", "court", "cover", "crabs", "crack", "craft", "crane", "crash", "crawl", "crazy", "cream", "creek", "creep", "crept", "crest", "crews", "cried", "cries", "crime", "crisp", "crops", "cross", "crowd", "crown", "crude", "cruel", "crush", "crust", "cubes", "cubic", "curls", "curly", "curve", "cycle", "daddy", "daily", "dairy", "daisy", "dance", "dared", "dares", "dates", "deals", "death", "debts", "decay", "delay", "dense", "depth", "desks", "devil", "diary", "didnt", "dimly", "dirty", "disco", "disks", "ditch", "dived", "diver", "docks", "doing", "dolls", "dolly", "domes", "doors", "doubt", "dough", "dozen", "draft", "drain", "drama", "drank", "drawn", "draws", "dread", "dream", "dress", "dried", "drier", "drift", "drill", "drily", "drink", "drive", "drops", "drove", "drown", "drugs", "drums", "drunk", "ducks", "dunes", "dusty", "dutch", "dying", "eager", "eagle", "early", "earth", "easel", "eaten", "edged", "edges", "Egypt", "eight", "elbow", "elder", "elves", "empty", "ended", "enemy", "enjoy", "enter", "entry", "equal", "erase", "error", "essay", "event", "every", "exact", "exist", "exits", "extra", "fable", "faced", "faces", "facts", "faded", "fails", "faint", "fairy", "faith", "falls", "false", "fancy", "farms", "fatal", "fatty", "fault", "favor", "fears", "feast", "feeds", "feels", "fence", "ferns", "ferry", "fetch", "fever", "fewer", "field", "fiery", "fifth", "fifty", "fight", "fills", "films", "final", "finds", "finer", "fired", "fires", "first", "fists", "fixed", "flags", "flame", "flaps", "flash", "flask", "flats", "fleet", "flesh", "flies", "flint", "float", "flock", "flood", "floor", "flour", "flown", "flows", "fluff", "fluid", "flung", "flute", "focus", "foggy", "folds", "folks", "foods", "force", "forms", "forth", "forts", "forty", "found", "fours", "foxes", "frail", "frame", "freed", "fresh", "fried", "fries", "frogs", "front", "frost", "frown", "fruit", "fuels", "fully", "fumes", "funds", "funny", "furry", "gains", "gales", "games", "gases", "gates", "gauge", "gazed", "gears", "geese", "germs", "ghost", "giant", "giddy", "gifts", "girls", "given", "gives", "glare", "glass", "glide", "globe", "glory", "glove", "glued", "glues", "goals", "goats", "going", "goods", "goose", "grabs", "grace", "grade", "grain", "grand", "grant", "graph", "grasp", "grass", "grave", "graze", "great", "Greek", "green", "greys", "grief", "grind", "grins", "group", "grove", "growl", "grown", "grows", "guard", "guess", "guest", "guide", "gulls", "habit", "hadnt", "hairs", "halls", "hands", "handy", "hangs", "happy", "harms", "harsh", "hasnt", "haste", "hatch", "hated", "hates", "haunt", "heads", "heals", "heard", "hears", "heart", "heats", "heavy", "heels", "hello", "helps", "hence", "herbs", "herds", "heres", "hides", "hills", "hilly", "hired", "hobby", "holds", "holes", "homes", "honey", "Honor", "hooks", "hoped", "hopes", "horns", "horse", "hotel", "hours", "house", "human", "humor", "hunts", "hurry", "hurts", "hutch", "ideal", "ideas", "image", "index", "India", "inner", "Irish", "irons", "issue", "Italy", "items", "japan", "jeans", "jello", "Jesus", "jewel", "joins", "joint", "jokes", "jolly", "judge", "juice", "juicy", "jumps", "keeps", "kicks", "kills", "kinds", "kings", "kites", "knees", "knelt", "knife", "knits", "knobs", "knock", "knots", "known", "knows", "label", "labor", "lakes", "lambs", "lamps", "lands", "lanes", "large", "lasts", "later", "Latin", "laugh", "lawns", "layer", "leads", "leaps", "learn", "least", "leave", "ledge", "legal", "lemon", "lends", "level", "lever", "lifts", "light", "liked", "likes", "limbs", "limit", "lined", "linen", "lines", "links", "lions", "lists", "liter", "lived", "liver", "lives", "loads", "local", "locks", "lodge", "lofty", "looks", "loops", "loose", "lords", "loser", "loses", "loved", "lover", "loves", "lower", "loyal", "lucky", "lumps", "lunar", "lunch", "lungs", "lying", "magic", "maids", "mails", "major", "maker", "makes", "males", "maple", "march", "marks", "marry", "marsh", "masks", "match", "maybe", "mayor", "meals", "means", "meats", "medal", "media", "meets", "melts", "mends", "mercy", "merry", "metal", "meter", "metre", "midst", "might", "miles", "milky", "mills", "mimes", "minds", "mined", "miner", "minor", "minus", "mists", "mixed", "mixes", "model", "moist", "molds", "moles", "money", "month", "moods", "moons", "moose", "moral", "moths", "motor", "mound", "mount", "mouse", "mouth", "moved", "moves", "movie", "muddy", "mules", "mummy", "music", "myths", "nails", "naked", "named", "names", "nanny", "nasty", "naval", "necks", "needs", "nerve", "nests", "never", "newer", "newly", "nicer", "night", "nines", "ninth", "noble", "noise", "noisy", "noone", "north", "noses", "nosey", "noted", "notes", "nouns", "novel", "nurse", "nylon", "oasis", "obeys", "occur", "ocean", "oddly", "offer", "often", "oiled", "older", "olive", "onion", "opens", "opera", "orbit", "order", "organ", "other", "ought", "ounce", "outer", "ovens", "owned", "owner", "packs", "pages", "pains", "paint", "pairs", "palms", "panel", "panic", "pants", "paper", "parks", "parts", "party", "paste", "patch", "paths", "pause", "paved", "peace", "peach", "peaks", "pearl", "pears", "pedal", "peels", "peeps", "pence", "penny", "perch", "phase", "phone", "photo", "piano", "picks", "piece", "piled", "piles", "pills", "pilot", "pinch", "pines", "pipes", "pitch", "place", "plain", "plait", "plane", "plank", "plans", "plant", "plate", "plays", "plows", "pluck", "plump", "poems", "poets", "point", "poked", "polar", "poles", "ponds", "pools", "porch", "ports", "posts", "pound", "pours", "power", "press", "price", "pride", "prime", "print", "prism", "prize", "proof", "proud", "prove", "puffs", "pulls", "pulse", "pumps", "punch", "pupil", "puppy", "purse", "pussy", "quack", "queen", "queer", "queue", "quick", "quiet", "quilt", "quite", "raced", "races", "racks", "radar", "radio", "rafts", "rails", "rains", "rainy", "raise", "rally", "ranch", "range", "ranks", "rapid", "rates", "ratio", "razor", "reach", "react", "reads", "ready", "reeds", "refer", "reign", "reins", "relax", "relay", "reply", "rests", "rhyme", "rider", "rides", "ridge", "rifle", "right", "rigid", "rings", "rinks", "riots", "ripen", "rises", "rival", "river", "roads", "roars", "roast", "robes", "robin", "robot", "rocks", "rocky", "rolls", "roman", "roofs", "rooms", "roots", "roped", "ropes", "roses", "rough", "round", "route", "royal", "ruins", "ruled", "ruler", "rules", "rungs", "rural", "rusty", "sacks", "sadly", "safer", "sails", "salad", "sales", "salty", "sands", "sandy", "sauce", "saved", "saves", "scale", "scare", "scarf", "scary", "scene", "scent", "score", "scout", "scrap", "screw", "scrub", "seals", "seats", "seeds", "seems", "seize", "sells", "sends", "sense", "serve", "seven", "sewed", "shade", "shady", "shaft", "shake", "shall", "shame", "shape", "share", "shark", "sharp", "sheds", "sheep", "sheer", "sheet", "shelf", "shell", "shift", "shine", "shiny", "ships", "shirt", "shock", "shoes", "shone", "shook", "shoot", "shops", "short", "shots", "shout", "shown", "shows", "shuts", "sides", "sight", "signs", "silks", "silky", "silly", "since", "sings", "sinks", "sirup", "sixes", "sixth", "sixty", "sized", "sizes", "skate", "skied", "skies", "skill", "skins", "skips", "skirt", "skull", "skunk", "slant", "slave", "sleek", "sleep", "slept", "slice", "slide", "slips", "slope", "slows", "smack", "small", "smart", "smash", "smell", "smelt", "smile", "smoke", "snack", "snail", "snake", "snaps", "sneak", "snows", "snowy", "socks", "soils", "solar", "solid", "solve", "songs", "sorry", "sorts", "sound", "south", "space", "spade", "Spain", "spare", "spark", "speak", "spear", "speed", "spell", "spelt", "spend", "spent", "sperm", "spill", "spine", "spins", "spite", "split", "spoil", "spoke", "spoon", "sport", "spots", "spray", "stack", "staff", "stage", "stair", "stalk", "stall", "stamp", "stand", "stare", "stars", "start", "state", "stays", "steak", "steal", "steam", "steel", "steep", "steer", "stems", "steps", "stern", "stick", "stiff", "still", "sting", "stirs", "stock", "stole", "stone", "stony", "stood", "stool", "stoop", "stops", "store", "storm", "story", "stout", "stove", "strap", "straw", "stray", "strip", "stuck", "study", "stuff", "stump", "stung", "stuns", "stunt", "style", "sugar", "suits", "sunny", "super", "swamp", "swear", "sweep", "sweet", "swell", "swept", "swift", "swims", "swing", "Swiss", "sword", "swung", "table", "tails", "taken", "takes", "tales", "talks", "tanks", "tarts", "tasks", "taste", "tasty", "taxes", "teach", "teams", "tears", "tease", "teddy", "teeth", "tells", "tempo", "tends", "tense", "tenth", "tents", "terms", "tests", "thank", "their", "theme", "there", "these", "theyd", "thick", "thief", "thing", "think", "third", "thorn", "those", "three", "threw", "throw", "thumb", "thump", "tidal", "tides", "tiger", "tight", "tiles", "timer", "times", "timid", "tired", "tires", "title", "toads", "toast", "today", "tones", "tools", "tooth", "topic", "torch", "total", "touch", "tough", "towel", "tower", "towns", "trace", "track", "trade", "trail", "train", "tramp", "traps", "treat", "trees", "trend", "trial", "tribe", "trick", "tried", "tries", "trips", "troop", "trout", "truck", "truly", "trunk", "trust", "truth", "tubes", "tummy", "tunes", "turns", "tusks", "twice", "twigs", "twins", "twist", "tying", "types", "tyres", "uncle", "under", "union", "units", "unity", "untie", "until", "upper", "upset", "urban", "urged", "using", "usual", "utter", "vague", "value", "valve", "Vapor", "vases", "veins", "venus", "verbs", "verse", "video", "views", "vines", "virus", "visit", "vital", "vivid", "vocal", "voice", "voted", "votes", "vowel", "waded", "wages", "wagon", "waist", "waits", "wakes", "walks", "walls", "wants", "warms", "wasnt", "waste", "watch", "water", "waved", "waves", "wears", "weary", "weave", "weeds", "weeks", "weigh", "weird", "wells", "whale", "wheat", "wheel", "where", "which", "while", "white", "whole", "whose", "wider", "widow", "width", "winds", "windy", "wings", "wiped", "wipes", "wired", "wires", "witch", "wives", "woman", "women", "woods", "words", "works", "world", "worms", "worry", "worse", "worst", "worth", "would", "wound", "woven", "wraps", "wreck", "wrist", "write", "wrong", "wrote", "xrays", "yacht", "yards", "yawns", "years", "yield", "yolks", "youll", "young", "youre", "yours", "youth", "youve", "yoyos", "zebra"},ResourceData[ResourceObject["Wordle Word List"]]]],
	i,n,keys={},word="",letters={},correct={},history={},
	green={},gray={},yellow={},colored={},def={},len=True,realword=True,
	temp=ConstantArray["",5],tempgreen={},tempyellow={},
	(*dgy=RGBColor["#3a3a3c"],*)dgy=GrayLevel[0.375],dyw=RGBColor["#eab308"],dgn=RGBColor["#63aa55"],lgy=GrayLevel[0.675],
	md,fr=None
	},

	n=RandomInteger[{1,Length[list]}];
	word=list[[n]];
	(* word=s; *)
	def=WordDefinition[word];
	md=Inactive[MessageDialog[
		Column[{
			Style["Definition(s):", FontWeight -> Bold],
			Column[Table["     " <> ToString[i] <> ". " <> def[[i]],{i,1,Length[def]}]]
		}]
	]];
	word=ToLowerCase[word];
	letters=Characters[word];
	(* Echo[word]; *)
	
	keys={
		{"q","w","e","r","t","y","u","i","o","p"},
		{"a","s","d","f","g","h","j","k","l"},
		{"z","x","c","v","b","n","m"}
	}/.{a_String:>Highlighted[Style[a,White,FontWeight->Bold,FontSize->16,FontFamily->"Arial"],Background->lgy,ImageSize->40,Frame->True,FrameStyle->Black]};
	keys=Column[keys,Alignment->{Center,Center}];

Manipulate[
	main[str_String,g_Integer]:=Module[{
		guessletters
	},
		guessnumber+=1;
		guessletters=Characters[ToLowerCase[str]];
		correct=Table[letters[[i]]==guessletters[[i]],{i,1,5}];
		temp=ConstantArray["",5];tempgreen={};tempyellow={};
				
		For[i=1,i<=Length[guessletters],i++,
			If[guessletters[[i]]===letters[[i]],
			(
				temp[[i]]="Green";
				AppendTo[tempgreen,guessletters[[i]]];
			)
			]
		];
		
		For[i=1,i<=Length[guessletters],i++,
			If[temp[[i]]!="Green",
				If[!MemberQ[letters,guessletters[[i]]]||(MemberQ[letters,guessletters[[i]]]&&Count[guessletters,guessletters[[i]]]>Count[letters,guessletters[[i]]]&&MemberQ[tempgreen,guessletters[[i]]])||(MemberQ[letters,guessletters[[i]]]&&Count[guessletters,guessletters[[i]]]>Count[letters,guessletters[[i]]]&&MemberQ[tempyellow,guessletters[[i]]]),
					temp[[i]]="Gray",
					(
						temp[[i]]="Yellow";
						AppendTo[tempyellow,guessletters[[i]]];
					)
				];
			];
		];
		
		For[i=1,i<=Length[guessletters],i++,
			If[temp[[i]]=="Green",
			(
				AppendTo[green,guessletters[[i]]];
				AppendTo[colored,style[guessletters[[i]],dgn]]
			),
			(
				If[temp[[i]]=="Yellow",
				(
					AppendTo[yellow,guessletters[[i]]];
					AppendTo[colored,style[guessletters[[i]],dyw]]
				),
				(
					AppendTo[gray,guessletters[[i]]];
					AppendTo[colored,style[guessletters[[i]],dgy]]
				)
				]
			)
			];
		];

		For[i=1,i<=Length[gray],i++,
			keys=(keys/.{Highlighted[Style[gray[[i]],opts___],opts2___]:>gray[[i]]})/.{gray[[i]]->Highlighted[Style[gray[[i]],White,FontWeight->Bold,FontSize->16,FontFamily->"Arial"],Background->dgy,ImageSize->40,Frame->True,FrameStyle->Black]};
		];

		For[i=1,i<=Length[yellow],i++,
			keys=(keys/.{Highlighted[Style[yellow[[i]],opts___],opts2___]:>yellow[[i]]})/.{yellow[[i]]->Highlighted[Style[yellow[[i]],White,FontWeight->Bold,FontSize->16,FontFamily->"Arial"],Background->dyw,ImageSize->40,Frame->True,FrameStyle->Black]};
		];
		
		For[i=1,i<=Length[green],i++,
			keys=(keys/.{Highlighted[Style[green[[i]],opts___],opts2___]:>green[[i]]})/.{green[[i]]->Highlighted[Style[green[[i]],White,FontWeight->Bold,FontSize->16,FontFamily->"Arial"],Background->dgn,ImageSize->40,Frame->True,FrameStyle->Black]};
		];
		
		If[0<=guesses,
		(
			PrependTo[colored,Style[Row[{guessnumber,". "},BaselinePosition->Bottom],FontFamily->"Arial",FontSize->18]];
			AppendTo[history,colored];
			colored={};
			(* green={};yellow={};gray={}; *)
			(* If you clear these out each time, the keyboard update is wrong! *)
		)
		];
		
		If[str==word,
		(
			done=True;			
		),
		(
			Return[main[x,g-1]]
		)
		];
	];
	
	Grid[{
	{
		""
	},
	{
		If[ToString[x] != word && guesses > 0,
			Row[{
				Style["Guesses remaining: ",FontWeight->Bold,FontFamily->"Arial",FontSize->32],
				Style[guesses,FontFamily->"Arial",FontSize->32]
			}],
			
			If[ToString[x]==word,
				If[!MissingQ[def],
					Row[{
						Style[Row[{Style["Word: ",FontWeight->Bold], word}],Darker[Green],FontFamily->"Arial",FontSize->32,TextAlignment->{Center, Center}],
						" ",
						Button[
							"?",
							Activate[md], 
							BaselinePosition->Bottom,Alignment->Center,Appearance->None,BaseStyle->{Darker[Green]}
						]
					}],
					Style[Row[{Style["Word: ",FontWeight->Bold],word}],Darker[Green],FontFamily->"Arial",FontSize->32,TextAlignment->{Center, Center}]
				],
				
				If[guesses == 0,
					If[!MissingQ[def],
						Row[{
							Style[Row[{Style["Word: ",FontWeight->Bold],word}],Red,FontFamily->"Arial",FontSize->32,TextAlignment->{Center, Center}],
							" ",
						Button[
							"?",
							Activate[md], 
							BaselinePosition->Bottom,Alignment->Center,Appearance->None,BaseStyle->{Red}
						]
					}],
					Style[Row[{Style["Word: ",FontWeight->Bold],word}],Red,FontFamily->"Arial",FontSize->32,TextAlignment->{Center, Center}]
					]
				]
			]
		]
	},
	{
		"\n"
	},
	{
		Grid[{
		{
			InputField[Dynamic[x],FieldHint->"Enter your guess",FieldSize->{20,1},BaselinePosition->Scaled[0],BaseStyle->{FontSize->16,FontFamily->"Arial"}],
			"\t",
			"\t",
			If[(ToString[x]!=word && guesses>0 && guessnumber > 0) || (ToString[x]==word) || (guesses==0),
				Grid[history,BaselinePosition->{{1,1},Bottom},Frame->fr]
			]
		}
		},Frame->fr]
	},
	{
		"\n"
	},
	{
		Column[
			Row[Riffle[keys[[1,#]]," "]]&/@Range[1,3]
		, Alignment->Center,Frame->fr]
	},
	{
		"\n"
	}
	},Frame->fr],
	
	Row[{
		Button[
			"Submit Guess",
			{
				If[StringLength[ToString[x]]==5,len=True,(len=False;MessageDialog["Guesses must have exactly five letters. Your guess of \"" <> ToLowerCase[ToString[x]] <> "\" has " <> ToString[StringLength[ToString[x]]] <> " letters."])];
				If[MemberQ[list2,ToLowerCase[ToString[x]]],realword=True,(realword=False;If[len==True,MessageDialog["Guesses must consist of valid dictionary words. Your guess of \"" <> ToLowerCase[ToString[x]] <> "\" is not in the dictionary."]])];
				If[done==False&&guesses>0&&len==True&&realword==True,main[ToString[x],guesses--]];
			}
		],
		Button[
			"Reset",
			clear[]
		]
	}],
	
	{{x,Null},None},{{guesses,6},None},{{done,False},None},
	
	TrackedSymbols:>{x,guesses,done},
	
	Initialization:>(
		clear[]:=(
			done=False;
			x=Null;
			guesses=6;
			len=True;
			realword=True;
			guessnumber=0;
			n=RandomInteger[{1,Length[list]}];
			word=list[[n]];
			word=ToLowerCase[word];
			def=WordDefinition[word];
			md=Inactive[MessageDialog[
				Column[{
					Style["Definition(s):", FontWeight -> Bold],
					Column[Table["     " <> ToString[i] <> ". " <> def[[i]],{i,1,Length[def]}]]
				}]
			]];
			letters=Characters[word];
			correct={};history={};
			temp=ConstantArray["",5];tempgreen={};tempyellow={};
			keys={
				{"q","w","e","r","t","y","u","i","o","p"},
				{"a","s","d","f","g","h","j","k","l"},
				{"z","x","c","v","b","n","m"}
			}/.{a_String:>Highlighted[Style[a,White,FontWeight->Bold,FontSize->16,FontFamily->"Arial",TextAlignment->Center],Background->lgy,ImageSize->40,Frame->True,FrameStyle->Black]};
			keys=Column[keys,Alignment->{Center,Center}];
			green={};gray={};yellow={};colored={};
			dgy=GrayLevel[0.375];dyw=RGBColor["#eab308"];dgn=RGBColor["#63aa55"];lgy=GrayLevel[0.675];
		)
	)
, Alignment->Center,ContentSize->{700,Automatic},FrameLabel->"PlayWordle"]
]


(* ::Section:: *)
(*Author Notes*)


(* ::Item:: *)
(*Online sources suggest that the official Wordle game has a list of words it uses for the game (link) and a separate word list consisting of what it considers "valid guesses" (link).*)


(* ::Subitem:: *)
(*This source says that the daily puzzle words will never be pulled from the "valid guesses" list, but I have not taken the time to investigate the Wordle source code extensively enough to see whether this is true. Future versions may consist of tweaks along these lines.*)


(* ::Subitem:: *)
(*To the best of my knowledge, the resource data object Wordle Word List is a combination of all possible Wordle-related words (i.e. a \!\( *)
(*TagBox[*)
(*ButtonBox["Union",*)
(*BaseStyle->{"Link"},*)
(*ButtonData->"paclet:ref/Union",*)
(*ContentPadding->False],*)
(*MouseAppearanceTag["LinkHand"]]\) of the two above-mentioned word lists).*)


(* ::Item:: *)
(*There are plans for future versions to implement:*)


(* ::Subitem:: *)
(*allowing users to choose from different word lists (the Wordle one, WordList, and maybe others)*)


(* ::Subitem:: *)
(*adjusting the number of initial guesses*)


(* ::Subitem:: *)
(*specifying whether guesses of length len!=5 are allowed*)


(* ::Subitem:: *)
(*specifying whether non-list guesses are allowed*)


(* ::Subitem:: *)
(*(possibly) choosing formatting options (colors, etc.) for the game itself*)


(* ::Item:: *)
(*Special thanks to Bob Sandheinrich for finding a coloring error in a previous version of this resource.*)


(* ::Item:: *)
(*Despite its awesomeness, the recent blog implementation by David Reiss is totally independent from this resource. Even so, the author thanks David for his generous conversations concerning Wordle and this resource in particular.*)


(* ::Item:: *)
(*Many thanks to the WFR Reviewers for their feedback and improvement suggestions. The implementation is much better now than it was before review.*)
