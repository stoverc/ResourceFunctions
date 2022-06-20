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
	list=ToLowerCase[{"aback", "abase", "abate", "abbey", "abbot", "abhor", "abide", "abled", "abode", "abort", "about", "above", "abuse", "abyss", "acids", "acorn", "acres", "acrid", "acted", "actor", "acute", "adage", "adapt", "added", "adept", "admin", "admit", "adobe", "adopt", "adore", "adorn", "adult", "affix", "afire", "afoot", "afoul", "after", "again", "agape", "agate", "agent", "agile", "aging", "aglow", "agony", "agora", "agree", "ahead", "aider", "aimed", "aisle", "alarm", "album", "alert", "algae", "alibi", "alien", "align", "alike", "alive", "allay", "alley", "allot", "allow", "alloy", "aloft", "alone", "along", "aloof", "aloud", "alpha", "altar", "alter", "amass", "amaze", "amber", "amble", "amend", "amiss", "amity", "among", "ample", "amply", "amuse", "angel", "anger", "angle", "angry", "angst", "anime", "ankle", "annex", "annoy", "annul", "anode", "antic", "anvil", "aorta", "apart", "aphid", "aping", "apnea", "apple", "apply", "april", "apron", "aptly", "arabs", "arbor", "ardor", "areas", "arena", "argue", "arise", "armed", "armor", "aroma", "arose", "array", "arrow", "arson", "artsy", "ascot", "ashen", "ashes", "aside", "asked", "askew", "assay", "asset", "atlas", "atoll", "atoms", "atone", "attic", "audio", "audit", "augur", "aunts", "aunty", "avail", "avert", "avian", "avoid", "await", "awake", "award", "aware", "awash", "awful", "awoke", "axial", "axiom", "axion", "azure", "backs", "bacon", "badge", "badly", "bagel", "baggy", "baked", "baker", "bakes", "baler", "balls", "balmy", "banal", "bands", "bangs", "banjo", "banks", "barge", "barks", "barns", "baron", "basal", "based", "bases", "basic", "basil", "basin", "basis", "baste", "batch", "bathe", "baton", "batty", "bawdy", "bayou", "beach", "beads", "beady", "beaks", "beams", "beans", "beard", "bears", "beast", "beats", "beech", "beefy", "befit", "began", "begat", "beget", "begin", "begun", "being", "belch", "belie", "belle", "bells", "belly", "below", "belts", "bench", "bends", "beret", "berry", "berth", "beset", "betel", "bevel", "bezel", "bible", "bicep", "biddy", "bigot", "bikes", "bilge", "bills", "billy", "binge", "bingo", "biome", "birch", "birds", "birth", "bison", "bites", "bitty", "black", "blade", "blame", "bland", "blank", "blare", "blast", "blaze", "bleak", "bleat", "bleed", "bleep", "blend", "bless", "blimp", "blind", "blink", "bliss", "blitz", "bloat", "block", "bloke", "blond", "blood", "bloom", "blown", "blows", "bluer", "blues", "bluff", "blunt", "blurb", "blurt", "blush", "board", "boast", "boats", "bobby", "boils", "bombs", "bones", "boney", "bongo", "bonus", "booby", "books", "boost", "booth", "boots", "booty", "booze", "boozy", "borax", "bored", "borne", "bosom", "bossy", "botch", "bough", "boule", "bound", "bowed", "bowel", "bowls", "boxer", "boxes", "brace", "braid", "brain", "brake", "brand", "brash", "brass", "brave", "bravo", "brawl", "brawn", "bread", "break", "breed", "briar", "bribe", "brick", "bride", "brief", "brine", "bring", "brink", "briny", "brisk", "broad", "broil", "broke", "brood", "brook", "broom", "broth", "brown", "brunt", "brush", "brute", "buddy", "budge", "buggy", "bugle", "build", "built", "bulbs", "bulge", "bulky", "bulls", "bully", "bumps", "bumpy", "bunch", "bunks", "bunny", "burly", "burns", "burnt", "burst", "bused", "buses", "bushy", "butch", "butte", "buxom", "buyer", "bylaw", "cabal", "cabby", "cabin", "cable", "cacao", "cache", "cacti", "caddy", "cadet", "caged", "cages", "cagey", "cairn", "cakes", "calls", "camel", "cameo", "camps", "canal", "candy", "canny", "canoe", "canon", "caper", "caput", "carat", "cards", "cared", "cares", "cargo", "carol", "carry", "carts", "carve", "cases", "caste", "catch", "cater", "catty", "caulk", "cause", "caves", "cavil", "cease", "cedar", "cello", "cells", "chafe", "chaff", "chain", "chair", "chalk", "champ", "chant", "chaos", "chard", "charm", "chart", "chase", "chasm", "cheap", "cheat", "check", "cheek", "cheer", "chess", "chest", "chick", "chide", "chief", "child", "chili", "chill", "chime", "china", "chips", "chirp", "chock", "choir", "choke", "chops", "chord", "chore", "chose", "chuck", "chump", "chunk", "churn", "chute", "cider", "cigar", "cinch", "circa", "civic", "civil", "clack", "claim", "clamp", "clams", "clang", "clank", "clash", "clasp", "class", "claws", "clean", "clear", "cleat", "cleft", "clerk", "click", "cliff", "climb", "cling", "clink", "cloak", "clock", "clone", "close", "cloth", "cloud", "clout", "clove", "clown", "clubs", "cluck", "clued", "clues", "clump", "clung", "coach", "coals", "coast", "coats", "cobra", "cocoa", "coins", "colon", "color", "comes", "comet", "comfy", "comic", "comma", "conch", "condo", "cones", "conic", "cooks", "cools", "copse", "coral", "cords", "corer", "corny", "costs", "couch", "cough", "could", "count", "coupe", "court", "coven", "cover", "covet", "covey", "cower", "coyly", "crabs", "crack", "craft", "cramp", "crane", "crank", "crash", "crass", "crate", "crave", "crawl", "craze", "crazy", "creak", "cream", "credo", "creed", "creek", "creep", "creme", "crepe", "crept", "cress", "crest", "crews", "crick", "cried", "crier", "cries", "crime", "crimp", "crisp", "croak", "crock", "crone", "crony", "crook", "crops", "cross", "croup", "crowd", "crown", "crude", "cruel", "crumb", "crump", "crush", "crust", "crypt", "cubes", "cubic", "cumin", "curio", "curls", "curly", "curry", "curse", "curve", "curvy", "cutie", "cyber", "cycle", "cynic", "daddy", "daily", "dairy", "daisy", "dally", "dance", "dandy", "dared", "dares", "dates", "datum", "daunt", "deals", "dealt", "death", "debar", "debit", "debts", "debug", "debut", "decal", "decay", "decor", "decoy", "decry", "defer", "deign", "deity", "delay", "delta", "delve", "demon", "demur", "denim", "dense", "depot", "depth", "derby", "desks", "deter", "detox", "deuce", "devil", "diary", "dicey", "digit", "dilly", "dimly", "diner", "dingo", "dingy", "diode", "dirge", "dirty", "disco", "disks", "ditch", "ditto", "ditty", "dived", "diver", "dizzy", "docks", "dodge", "dodgy", "dogma", "doing", "dolls", "dolly", "domes", "donor", "donut", "doors", "dopey", "doubt", "dough", "dowdy", "dowel", "downy", "dowry", "dozen", "draft", "drain", "drake", "drama", "drank", "drape", "drawl", "drawn", "draws", "dread", "dream", "dress", "dried", "drier", "drift", "drill", "drily", "drink", "drive", "droit", "droll", "drone", "drool", "droop", "drops", "dross", "drove", "drown", "drugs", "druid", "drums", "drunk", "dryer", "dryly", "duchy", "ducks", "dully", "dummy", "dumpy", "dunce", "dunes", "dusky", "dusty", "dutch", "duvet", "dwarf", "dwell", "dwelt", "dying", "eager", "eagle", "early", "earth", "easel", "eaten", "eater", "ebony", "eclat", "edged", "edges", "edict", "edify", "eerie", "egret", "egypt", "eight", "eject", "eking", "elate", "elbow", "elder", "elect", "elegy", "elfin", "elide", "elite", "elope", "elude", "elves", "email", "embed", "ember", "emcee", "empty", "enact", "ended", "endow", "enema", "enemy", "enjoy", "ennui", "ensue", "enter", "entry", "envoy", "epoch", "epoxy", "equal", "equip", "erase", "erect", "erode", "error", "erupt", "essay", "ester", "ether", "ethic", "ethos", "etude", "evade", "event", "every", "evict", "evoke", "exact", "exalt", "excel", "exert", "exile", "exist", "exits", "expel", "extol", "extra", "exult", "eying", "fable", "faced", "faces", "facet", "facts", "faded", "fails", "faint", "fairy", "faith", "falls", "false", "fancy", "fanny", "farce", "farms", "fatal", "fatty", "fault", "fauna", "favor", "fears", "feast", "fecal", "feeds", "feels", "feign", "fella", "felon", "femme", "femur", "fence", "feral", "ferns", "ferry", "fetal", "fetch", "fetid", "fetus", "fever", "fewer", "fiber", "fibre", "ficus", "field", "fiend", "fiery", "fifth", "fifty", "fight", "filer", "filet", "fills", "filly", "films", "filmy", "filth", "final", "finch", "finds", "finer", "fired", "fires", "first", "fishy", "fists", "fixed", "fixer", "fizzy", "fjord", "flack", "flags", "flail", "flair", "flake", "flaky", "flame", "flank", "flaps", "flare", "flash", "flask", "flats", "fleck", "fleet", "flesh", "flick", "flier", "flies", "fling", "flint", "flirt", "float", "flock", "flood", "floor", "flora", "floss", "flour", "flout", "flown", "flows", "fluff", "fluid", "fluke", "flume", "flung", "flunk", "flush", "flute", "flyer", "foamy", "focal", "focus", "foggy", "foist", "folds", "folio", "folks", "folly", "foods", "foray", "force", "forge", "forgo", "forms", "forte", "forth", "forts", "forty", "forum", "found", "fours", "foxes", "foyer", "frail", "frame", "frank", "fraud", "freak", "freed", "freer", "fresh", "friar", "fried", "fries", "frill", "frisk", "fritz", "frock", "frogs", "frond", "front", "frost", "froth", "frown", "froze", "fruit", "fudge", "fuels", "fugue", "fully", "fumes", "funds", "fungi", "funky", "funny", "furor", "furry", "fussy", "fuzzy", "gaffe", "gaily", "gains", "gales", "gamer", "games", "gamma", "gamut", "gases", "gassy", "gates", "gaudy", "gauge", "gaunt", "gauze", "gavel", "gawky", "gayer", "gayly", "gazed", "gazer", "gears", "gecko", "geeky", "geese", "genie", "genre", "germs", "ghost", "ghoul", "giant", "giddy", "gifts", "gipsy", "girls", "girly", "girth", "given", "giver", "gives", "glade", "gland", "glare", "glass", "glaze", "gleam", "glean", "glide", "glint", "gloat", "globe", "gloom", "glory", "gloss", "glove", "glued", "glues", "glyph", "gnash", "gnome", "goals", "goats", "godly", "going", "golem", "golly", "gonad", "goner", "goods", "goody", "gooey", "goofy", "goose", "gorge", "gouge", "gourd", "grabs", "grace", "grade", "graft", "grail", "grain", "grand", "grant", "grape", "graph", "grasp", "grass", "grate", "grave", "gravy", "graze", "great", "greed", "greek", "green", "greet", "greys", "grief", "grill", "grime", "grimy", "grind", "grins", "gripe", "groan", "groin", "groom", "grope", "gross", "group", "grout", "grove", "growl", "grown", "grows", "gruel", "gruff", "grunt", "guard", "guava", "guess", "guest", "guide", "guild", "guile", "guilt", "guise", "gulch", "gulls", "gully", "gumbo", "gummy", "guppy", "gusto", "gusty", "gypsy", "habit", "hairs", "hairy", "halls", "halve", "hands", "handy", "hangs", "happy", "hardy", "harem", "harms", "harpy", "harry", "harsh", "haste", "hasty", "hatch", "hated", "hater", "hates", "haunt", "haute", "haven", "havoc", "hazel", "heads", "heady", "heals", "heard", "hears", "heart", "heath", "heats", "heave", "heavy", "hedge", "heels", "hefty", "heist", "helix", "hello", "helps", "hence", "herbs", "herds", "heres", "heron", "hides", "hills", "hilly", "hinge", "hippo", "hippy", "hired", "hitch", "hoard", "hobby", "hoist", "holds", "holes", "holly", "homer", "homes", "honey", "honor", "hooks", "hoped", "hopes", "horde", "horns", "horny", "horse", "hotel", "hotly", "hound", "hours", "house", "hovel", "hover", "howdy", "human", "humid", "humor", "humph", "humus", "hunch", "hunky", "hunts", "hurry", "hurts", "husky", "hussy", "hutch", "hydro", "hyena", "hymen", "hyper", "icily", "icing", "ideal", "ideas", "idiom", "idiot", "idler", "idyll", "igloo", "iliac", "image", "imbue", "impel", "imply", "inane", "inbox", "incur", "index", "india", "inept", "inert", "infer", "ingot", "inlay", "inlet", "inner", "input", "inter", "intro", "ionic", "irate", "irish", "irons", "irony", "islet", "issue", "italy", "itchy", "items", "ivory", "japan", "jaunt", "jazzy", "jeans", "jello", "jelly", "jerky", "jesus", "jetty", "jewel", "jiffy", "joins", "joint", "joist", "joker", "jokes", "jolly", "joust", "judge", "juice", "juicy", "jumbo", "jumps", "jumpy", "junta", "junto", "juror", "kappa", "karma", "kayak", "kebab", "keeps", "khaki", "kicks", "kills", "kinds", "kings", "kinky", "kiosk", "kites", "kitty", "knack", "knave", "knead", "kneed", "kneel", "knees", "knelt", "knife", "knits", "knobs", "knock", "knoll", "knots", "known", "knows", "koala", "krill", "label", "labor", "laden", "ladle", "lager", "lakes", "lambs", "lamps", "lance", "lands", "lanes", "lanky", "lapel", "lapse", "large", "larva", "lasso", "lasts", "latch", "later", "lathe", "latin", "latte", "laugh", "lawns", "layer", "leach", "leads", "leafy", "leaky", "leant", "leaps", "leapt", "learn", "lease", "leash", "least", "leave", "ledge", "leech", "leery", "lefty", "legal", "leggy", "lemon", "lemur", "lends", "leper", "level", "lever", "libel", "liege", "lifts", "light", "liked", "liken", "likes", "lilac", "limbo", "limbs", "limit", "lined", "linen", "liner", "lines", "lingo", "links", "lions", "lipid", "lists", "liter", "lithe", "lived", "liver", "lives", "livid", "llama", "loads", "loamy", "loath", "lobby", "local", "locks", "locus", "lodge", "lofty", "logic", "login", "looks", "loops", "loopy", "loose", "lords", "lorry", "loser", "loses", "louse", "lousy", "loved", "lover", "loves", "lower", "lowly", "loyal", "lucid", "lucky", "lumen", "lumps", "lumpy", "lunar", "lunch", "lunge", "lungs", "lupus", "lurch", "lurid", "lusty", "lying", "lymph", "lynch", "lyric", "macaw", "macho", "macro", "madam", "madly", "mafia", "magic", "magma", "maids", "mails", "maize", "major", "maker", "makes", "males", "mambo", "mamma", "mammy", "manga", "mange", "mango", "mangy", "mania", "manic", "manly", "manor", "maple", "march", "marks", "marry", "marsh", "masks", "mason", "masse", "match", "matey", "mauve", "maxim", "maybe", "mayor", "meals", "mealy", "means", "meant", "meats", "meaty", "mecca", "medal", "media", "medic", "meets", "melee", "melon", "melts", "mends", "mercy", "merge", "merit", "merry", "metal", "meter", "metre", "metro", "micro", "midge", "midst", "might", "miles", "milky", "mills", "mimes", "mimic", "mince", "minds", "mined", "miner", "minim", "minor", "minty", "minus", "mirth", "miser", "missy", "mists", "mixed", "mixes", "mocha", "modal", "model", "modem", "mogul", "moist", "molar", "molds", "moldy", "moles", "money", "month", "moods", "moody", "moons", "moose", "moral", "moron", "morph", "mossy", "motel", "moths", "motif", "motor", "motto", "moult", "mound", "mount", "mourn", "mouse", "mouth", "moved", "mover", "moves", "movie", "mower", "mucky", "mucus", "muddy", "mulch", "mules", "mummy", "munch", "mural", "murky", "mushy", "music", "musky", "musty", "myrrh", "myths", "nadir", "nails", "naive", "naked", "named", "names", "nanny", "nasal", "nasty", "natal", "naval", "navel", "necks", "needs", "needy", "neigh", "nerdy", "nerve", "nests", "never", "newer", "newly", "nicer", "niche", "niece", "night", "nines", "ninja", "ninny", "ninth", "noble", "nobly", "noise", "noisy", "nomad", "noone", "noose", "north", "noses", "nosey", "notch", "noted", "notes", "nouns", "novel", "nudge", "nurse", "nutty", "nylon", "nymph", "oaken", "oasis", "obese", "obeys", "occur", "ocean", "octal", "octet", "odder", "oddly", "offal", "offer", "often", "oiled", "olden", "older", "olive", "ombre", "omega", "onion", "onset", "opens", "opera", "opine", "opium", "optic", "orbit", "order", "organ", "other", "otter", "ought", "ounce", "outdo", "outer", "outgo", "ovary", "ovate", "ovens", "overt", "ovine", "ovoid", "owing", "owned", "owner", "oxide", "ozone", "packs", "paddy", "pagan", "pages", "pains", "paint", "pairs", "paler", "palms", "palsy", "panel", "panic", "pansy", "pants", "papal", "paper", "parer", "parka", "parks", "parry", "parse", "parts", "party", "pasta", "paste", "pasty", "patch", "paths", "patio", "patsy", "patty", "pause", "paved", "payee", "payer", "peace", "peach", "peaks", "pearl", "pears", "pecan", "pedal", "peels", "peeps", "penal", "pence", "penne", "penny", "perch", "peril", "perky", "pesky", "pesto", "petal", "petty", "phase", "phone", "phony", "photo", "piano", "picks", "picky", "piece", "piety", "piggy", "piled", "piles", "pills", "pilot", "pinch", "pines", "piney", "pinky", "pinto", "piper", "pipes", "pique", "pitch", "pithy", "pivot", "pixel", "pixie", "pizza", "place", "plaid", "plain", "plait", "plane", "plank", "plans", "plant", "plate", "plays", "plaza", "plead", "pleat", "plied", "plier", "plows", "pluck", "plumb", "plume", "plump", "plunk", "plush", "poems", "poesy", "poets", "point", "poise", "poked", "poker", "polar", "poles", "polka", "polyp", "ponds", "pooch", "pools", "poppy", "porch", "ports", "poser", "posit", "posse", "posts", "pouch", "pound", "pours", "pouty", "power", "prank", "prawn", "preen", "press", "price", "prick", "pride", "pried", "prime", "primo", "print", "prior", "prism", "privy", "prize", "probe", "prone", "prong", "proof", "prose", "proud", "prove", "prowl", "proxy", "prude", "prune", "psalm", "pubic", "pudgy", "puffs", "puffy", "pulls", "pulpy", "pulse", "pumps", "punch", "pupal", "pupil", "puppy", "puree", "purer", "purge", "purse", "pushy", "putty", "pygmy", "quack", "quail", "quake", "qualm", "quark", "quart", "quash", "quasi", "queen", "queer", "quell", "query", "quest", "queue", "quick", "quiet", "quill", "quilt", "quirk", "quite", "quota", "quote", "quoth", "rabbi", "rabid", "raced", "racer", "races", "racks", "radar", "radii", "radio", "rafts", "rails", "rains", "rainy", "raise", "rajah", "rally", "ralph", "ramen", "ranch", "randy", "range", "ranks", "rapid", "rarer", "raspy", "rates", "ratio", "ratty", "raven", "rayon", "razor", "reach", "react", "reads", "ready", "realm", "rearm", "rebar", "rebel", "rebus", "rebut", "recap", "recur", "recut", "reeds", "reedy", "refer", "refit", "regal", "rehab", "reign", "reins", "relax", "relay", "relic", "remit", "renal", "renew", "repay", "repel", "reply", "rerun", "reset", "resin", "rests", "retch", "retro", "retry", "reuse", "revel", "revue", "rhino", "rhyme", "rider", "rides", "ridge", "rifle", "right", "rigid", "rigor", "rings", "rinks", "rinse", "riots", "ripen", "riper", "risen", "riser", "rises", "risky", "rival", "river", "rivet", "roach", "roads", "roars", "roast", "robes", "robin", "robot", "rocks", "rocky", "rodeo", "roger", "rogue", "rolls", "roman", "roofs", "rooms", "roomy", "roost", "roots", "roped", "ropes", "roses", "rotor", "rouge", "rough", "round", "rouse", "route", "rover", "rowdy", "rower", "royal", "ruddy", "ruder", "rugby", "ruins", "ruled", "ruler", "rules", "rumba", "rumor", "rungs", "rupee", "rural", "rusty", "sacks", "sadly", "safer", "sails", "saint", "salad", "sales", "sally", "salon", "salsa", "salty", "salve", "salvo", "sands", "sandy", "saner", "sappy", "sassy", "satin", "satyr", "sauce", "saucy", "sauna", "saute", "saved", "saves", "savor", "savoy", "savvy", "scald", "scale", "scalp", "scaly", "scamp", "scant", "scare", "scarf", "scary", "scene", "scent", "scion", "scoff", "scold", "scone", "scoop", "scope", "score", "scorn", "scour", "scout", "scowl", "scram", "scrap", "scree", "screw", "scrub", "scrum", "scuba", "seals", "seats", "sedan", "seeds", "seedy", "seems", "segue", "seize", "sells", "semen", "sends", "sense", "sepia", "serif", "serum", "serve", "setup", "seven", "sever", "sewed", "sewer", "shack", "shade", "shady", "shaft", "shake", "shaky", "shale", "shall", "shalt", "shame", "shank", "shape", "shard", "share", "shark", "sharp", "shave", "shawl", "shear", "sheds", "sheen", "sheep", "sheer", "sheet", "sheik", "shelf", "shell", "shied", "shift", "shine", "shiny", "ships", "shire", "shirk", "shirt", "shoal", "shock", "shoes", "shone", "shook", "shoot", "shops", "shore", "shorn", "short", "shots", "shout", "shove", "shown", "shows", "showy", "shrew", "shrub", "shrug", "shuck", "shunt", "shush", "shuts", "shyly", "sides", "siege", "sieve", "sight", "sigma", "signs", "silks", "silky", "silly", "since", "sinew", "singe", "sings", "sinks", "siren", "sirup", "sissy", "sixes", "sixth", "sixty", "sized", "sizes", "skate", "skied", "skier", "skies", "skiff", "skill", "skimp", "skins", "skips", "skirt", "skulk", "skull", "skunk", "slack", "slain", "slang", "slant", "slash", "slate", "slave", "sleek", "sleep", "sleet", "slept", "slice", "slick", "slide", "slime", "slimy", "sling", "slink", "slips", "sloop", "slope", "slosh", "sloth", "slows", "slump", "slung", "slunk", "slurp", "slush", "slyly", "smack", "small", "smart", "smash", "smear", "smell", "smelt", "smile", "smirk", "smite", "smith", "smock", "smoke", "smoky", "smote", "snack", "snail", "snake", "snaky", "snaps", "snare", "snarl", "sneak", "sneer", "snide", "sniff", "snipe", "snoop", "snore", "snort", "snout", "snows", "snowy", "snuck", "snuff", "soapy", "sober", "socks", "soggy", "soils", "solar", "solid", "solve", "sonar", "songs", "sonic", "sooth", "sooty", "sorry", "sorts", "sound", "south", "sower", "space", "spade", "spain", "spank", "spare", "spark", "spasm", "spawn", "speak", "spear", "speck", "speed", "spell", "spelt", "spend", "spent", "sperm", "spice", "spicy", "spied", "spiel", "spike", "spiky", "spill", "spilt", "spine", "spins", "spiny", "spire", "spite", "splat", "split", "spoil", "spoke", "spoof", "spook", "spool", "spoon", "spore", "sport", "spots", "spout", "spray", "spree", "sprig", "spunk", "spurn", "spurt", "squad", "squat", "squib", "stack", "staff", "stage", "staid", "stain", "stair", "stake", "stale", "stalk", "stall", "stamp", "stand", "stank", "stare", "stark", "stars", "start", "stash", "state", "stave", "stays", "stead", "steak", "steal", "steam", "steed", "steel", "steep", "steer", "stein", "stems", "steps", "stern", "stick", "stiff", "still", "stilt", "sting", "stink", "stint", "stirs", "stock", "stoic", "stoke", "stole", "stomp", "stone", "stony", "stood", "stool", "stoop", "stops", "store", "stork", "storm", "story", "stout", "stove", "strap", "straw", "stray", "strip", "strut", "stuck", "study", "stuff", "stump", "stung", "stunk", "stuns", "stunt", "style", "suave", "sugar", "suing", "suite", "suits", "sulky", "sully", "sumac", "sunny", "super", "surer", "surge", "surly", "sushi", "swami", "swamp", "swarm", "swash", "swath", "swear", "sweat", "sweep", "sweet", "swell", "swept", "swift", "swill", "swims", "swine", "swing", "swirl", "swish", "swiss", "swoon", "swoop", "sword", "swore", "sworn", "swung", "synod", "syrup", "tabby", "table", "taboo", "tacit", "tacky", "taffy", "tails", "taint", "taken", "taker", "takes", "tales", "talks", "tally", "talon", "tamer", "tango", "tangy", "tanks", "taper", "tapir", "tardy", "tarot", "tarts", "tasks", "taste", "tasty", "tatty", "taunt", "tawny", "taxes", "teach", "teams", "tears", "teary", "tease", "teddy", "teeth", "tells", "tempo", "tends", "tenet", "tenor", "tense", "tenth", "tents", "tepee", "tepid", "terms", "terra", "terse", "tests", "testy", "thank", "theft", "their", "theme", "there", "these", "theta", "thick", "thief", "thigh", "thing", "think", "third", "thong", "thorn", "those", "three", "threw", "throb", "throw", "thrum", "thumb", "thump", "thyme", "tiara", "tibia", "tidal", "tides", "tiger", "tight", "tilde", "tiles", "timer", "times", "timid", "tipsy", "tired", "tires", "titan", "tithe", "title", "toads", "toast", "today", "toddy", "token", "tonal", "tones", "tonga", "tonic", "tools", "tooth", "topaz", "topic", "torch", "torso", "torus", "total", "totem", "touch", "tough", "towel", "tower", "towns", "toxic", "toxin", "trace", "track", "tract", "trade", "trail", "train", "trait", "tramp", "traps", "trash", "trawl", "tread", "treat", "trees", "trend", "triad", "trial", "tribe", "trice", "trick", "tried", "tries", "tripe", "trips", "trite", "troll", "troop", "trope", "trout", "trove", "truce", "truck", "truer", "truly", "trump", "trunk", "truss", "trust", "truth", "tryst", "tubal", "tuber", "tubes", "tulip", "tulle", "tummy", "tumor", "tunes", "tunic", "turbo", "turns", "tusks", "tutor", "twang", "tweak", "tweed", "tweet", "twice", "twigs", "twine", "twins", "twirl", "twist", "twixt", "tying", "types", "tyres", "udder", "ulcer", "ultra", "umbra", "uncle", "uncut", "under", "undid", "undue", "unfed", "unfit", "unify", "union", "unite", "units", "unity", "unlit", "unmet", "unset", "untie", "until", "unwed", "unzip", "upper", "upset", "urban", "urged", "urine", "usage", "usher", "using", "usual", "usurp", "utile", "utter", "vague", "valet", "valid", "valor", "value", "valve", "vapid", "vapor", "vases", "vault", "vaunt", "vegan", "veins", "venom", "venue", "venus", "verbs", "verge", "verse", "verso", "verve", "vicar", "video", "views", "vigil", "vigor", "villa", "vines", "vinyl", "viola", "viper", "viral", "virus", "visit", "visor", "vista", "vital", "vivid", "vixen", "vocal", "vodka", "vogue", "voice", "voila", "vomit", "voted", "voter", "votes", "vouch", "vowel", "vying", "wacky", "waded", "wafer", "wager", "wages", "wagon", "waist", "waits", "waive", "wakes", "walks", "walls", "waltz", "wants", "warms", "warty", "waste", "watch", "water", "waved", "waver", "waves", "waxen", "wears", "weary", "weave", "wedge", "weeds", "weedy", "weeks", "weigh", "weird", "welch", "wells", "welsh", "wench", "whack", "whale", "wharf", "wheat", "wheel", "whelp", "where", "which", "whiff", "while", "whine", "whiny", "whirl", "whisk", "white", "whole", "whoop", "whose", "widen", "wider", "widow", "width", "wield", "wight", "willy", "wimpy", "wince", "winch", "winds", "windy", "wings", "wiped", "wipes", "wired", "wires", "wiser", "wispy", "witch", "witty", "wives", "woken", "woman", "women", "woods", "woody", "wooer", "wooly", "woozy", "words", "wordy", "works", "world", "worms", "worry", "worse", "worst", "worth", "would", "wound", "woven", "wrack", "wraps", "wrath", "wreak", "wreck", "wrest", "wring", "wrist", "write", "wrong", "wrote", "wrung", "wryly", "xrays", "yacht", "yards", "yawns", "yearn", "years", "yeast", "yield", "yolks", "young", "yours", "youth", "yoyos", "zebra", "zesty", "zonal"}],
	list2=ToLowerCase[Union[{"aback", "abase", "abate", "abbey", "abbot", "abhor", "abide", "abled", "abode", "abort", "about", "above", "abuse", "abyss", "acids", "acorn", "acres", "acrid", "acted", "actor", "acute", "adage", "adapt", "added", "adept", "admin", "admit", "adobe", "adopt", "adore", "adorn", "adult", "affix", "afire", "afoot", "afoul", "after", "again", "agape", "agate", "agent", "agile", "aging", "aglow", "agony", "agora", "agree", "ahead", "aider", "aimed", "aisle", "alarm", "album", "alert", "algae", "alibi", "alien", "align", "alike", "alive", "allay", "alley", "allot", "allow", "alloy", "aloft", "alone", "along", "aloof", "aloud", "alpha", "altar", "alter", "amass", "amaze", "amber", "amble", "amend", "amiss", "amity", "among", "ample", "amply", "amuse", "angel", "anger", "angle", "angry", "angst", "anime", "ankle", "annex", "annoy", "annul", "anode", "antic", "anvil", "aorta", "apart", "aphid", "aping", "apnea", "apple", "apply", "april", "apron", "aptly", "arabs", "arbor", "ardor", "areas", "arena", "argue", "arise", "armed", "armor", "aroma", "arose", "array", "arrow", "arson", "artsy", "ascot", "ashen", "ashes", "aside", "asked", "askew", "assay", "asset", "atlas", "atoll", "atoms", "atone", "attic", "audio", "audit", "augur", "aunts", "aunty", "avail", "avert", "avian", "avoid", "await", "awake", "award", "aware", "awash", "awful", "awoke", "axial", "axiom", "axion", "azure", "backs", "bacon", "badge", "badly", "bagel", "baggy", "baked", "baker", "bakes", "baler", "balls", "balmy", "banal", "bands", "bangs", "banjo", "banks", "barge", "barks", "barns", "baron", "basal", "based", "bases", "basic", "basil", "basin", "basis", "baste", "batch", "bathe", "baton", "batty", "bawdy", "bayou", "beach", "beads", "beady", "beaks", "beams", "beans", "beard", "bears", "beast", "beats", "beech", "beefy", "befit", "began", "begat", "beget", "begin", "begun", "being", "belch", "belie", "belle", "bells", "belly", "below", "belts", "bench", "bends", "beret", "berry", "berth", "beset", "betel", "bevel", "bezel", "bible", "bicep", "biddy", "bigot", "bikes", "bilge", "bills", "billy", "binge", "bingo", "biome", "birch", "birds", "birth", "bison", "bites", "bitty", "black", "blade", "blame", "bland", "blank", "blare", "blast", "blaze", "bleak", "bleat", "bleed", "bleep", "blend", "bless", "blimp", "blind", "blink", "bliss", "blitz", "bloat", "block", "bloke", "blond", "blood", "bloom", "blown", "blows", "bluer", "blues", "bluff", "blunt", "blurb", "blurt", "blush", "board", "boast", "boats", "bobby", "boils", "bombs", "bones", "boney", "bongo", "bonus", "booby", "books", "boost", "booth", "boots", "booty", "booze", "boozy", "borax", "bored", "borne", "bosom", "bossy", "botch", "bough", "boule", "bound", "bowed", "bowel", "bowls", "boxer", "boxes", "brace", "braid", "brain", "brake", "brand", "brash", "brass", "brave", "bravo", "brawl", "brawn", "bread", "break", "breed", "briar", "bribe", "brick", "bride", "brief", "brine", "bring", "brink", "briny", "brisk", "broad", "broil", "broke", "brood", "brook", "broom", "broth", "brown", "brunt", "brush", "brute", "buddy", "budge", "buggy", "bugle", "build", "built", "bulbs", "bulge", "bulky", "bulls", "bully", "bumps", "bumpy", "bunch", "bunks", "bunny", "burly", "burns", "burnt", "burst", "bused", "buses", "bushy", "butch", "butte", "buxom", "buyer", "bylaw", "cabal", "cabby", "cabin", "cable", "cacao", "cache", "cacti", "caddy", "cadet", "caged", "cages", "cagey", "cairn", "cakes", "calls", "camel", "cameo", "camps", "canal", "candy", "canny", "canoe", "canon", "caper", "caput", "carat", "cards", "cared", "cares", "cargo", "carol", "carry", "carts", "carve", "cases", "caste", "catch", "cater", "catty", "caulk", "cause", "caves", "cavil", "cease", "cedar", "cello", "cells", "chafe", "chaff", "chain", "chair", "chalk", "champ", "chant", "chaos", "chard", "charm", "chart", "chase", "chasm", "cheap", "cheat", "check", "cheek", "cheer", "chess", "chest", "chick", "chide", "chief", "child", "chili", "chill", "chime", "china", "chips", "chirp", "chock", "choir", "choke", "chops", "chord", "chore", "chose", "chuck", "chump", "chunk", "churn", "chute", "cider", "cigar", "cinch", "circa", "civic", "civil", "clack", "claim", "clamp", "clams", "clang", "clank", "clash", "clasp", "class", "claws", "clean", "clear", "cleat", "cleft", "clerk", "click", "cliff", "climb", "cling", "clink", "cloak", "clock", "clone", "close", "cloth", "cloud", "clout", "clove", "clown", "clubs", "cluck", "clued", "clues", "clump", "clung", "coach", "coals", "coast", "coats", "cobra", "cocoa", "coins", "colon", "color", "comes", "comet", "comfy", "comic", "comma", "conch", "condo", "cones", "conic", "cooks", "cools", "copse", "coral", "cords", "corer", "corny", "costs", "couch", "cough", "could", "count", "coupe", "court", "coven", "cover", "covet", "covey", "cower", "coyly", "crabs", "crack", "craft", "cramp", "crane", "crank", "crash", "crass", "crate", "crave", "crawl", "craze", "crazy", "creak", "cream", "credo", "creed", "creek", "creep", "creme", "crepe", "crept", "cress", "crest", "crews", "crick", "cried", "crier", "cries", "crime", "crimp", "crisp", "croak", "crock", "crone", "crony", "crook", "crops", "cross", "croup", "crowd", "crown", "crude", "cruel", "crumb", "crump", "crush", "crust", "crypt", "cubes", "cubic", "cumin", "curio", "curls", "curly", "curry", "curse", "curve", "curvy", "cutie", "cyber", "cycle", "cynic", "daddy", "daily", "dairy", "daisy", "dally", "dance", "dandy", "dared", "dares", "dates", "datum", "daunt", "deals", "dealt", "death", "debar", "debit", "debts", "debug", "debut", "decal", "decay", "decor", "decoy", "decry", "defer", "deign", "deity", "delay", "delta", "delve", "demon", "demur", "denim", "dense", "depot", "depth", "derby", "desks", "deter", "detox", "deuce", "devil", "diary", "dicey", "digit", "dilly", "dimly", "diner", "dingo", "dingy", "diode", "dirge", "dirty", "disco", "disks", "ditch", "ditto", "ditty", "dived", "diver", "dizzy", "docks", "dodge", "dodgy", "dogma", "doing", "dolls", "dolly", "domes", "donor", "donut", "doors", "dopey", "doubt", "dough", "dowdy", "dowel", "downy", "dowry", "dozen", "draft", "drain", "drake", "drama", "drank", "drape", "drawl", "drawn", "draws", "dread", "dream", "dress", "dried", "drier", "drift", "drill", "drily", "drink", "drive", "droit", "droll", "drone", "drool", "droop", "drops", "dross", "drove", "drown", "drugs", "druid", "drums", "drunk", "dryer", "dryly", "duchy", "ducks", "dully", "dummy", "dumpy", "dunce", "dunes", "dusky", "dusty", "dutch", "duvet", "dwarf", "dwell", "dwelt", "dying", "eager", "eagle", "early", "earth", "easel", "eaten", "eater", "ebony", "eclat", "edged", "edges", "edict", "edify", "eerie", "egret", "egypt", "eight", "eject", "eking", "elate", "elbow", "elder", "elect", "elegy", "elfin", "elide", "elite", "elope", "elude", "elves", "email", "embed", "ember", "emcee", "empty", "enact", "ended", "endow", "enema", "enemy", "enjoy", "ennui", "ensue", "enter", "entry", "envoy", "epoch", "epoxy", "equal", "equip", "erase", "erect", "erode", "error", "erupt", "essay", "ester", "ether", "ethic", "ethos", "etude", "evade", "event", "every", "evict", "evoke", "exact", "exalt", "excel", "exert", "exile", "exist", "exits", "expel", "extol", "extra", "exult", "eying", "fable", "faced", "faces", "facet", "facts", "faded", "fails", "faint", "fairy", "faith", "falls", "false", "fancy", "fanny", "farce", "farms", "fatal", "fatty", "fault", "fauna", "favor", "fears", "feast", "fecal", "feeds", "feels", "feign", "fella", "felon", "femme", "femur", "fence", "feral", "ferns", "ferry", "fetal", "fetch", "fetid", "fetus", "fever", "fewer", "fiber", "fibre", "ficus", "field", "fiend", "fiery", "fifth", "fifty", "fight", "filer", "filet", "fills", "filly", "films", "filmy", "filth", "final", "finch", "finds", "finer", "fired", "fires", "first", "fishy", "fists", "fixed", "fixer", "fizzy", "fjord", "flack", "flags", "flail", "flair", "flake", "flaky", "flame", "flank", "flaps", "flare", "flash", "flask", "flats", "fleck", "fleet", "flesh", "flick", "flier", "flies", "fling", "flint", "flirt", "float", "flock", "flood", "floor", "flora", "floss", "flour", "flout", "flown", "flows", "fluff", "fluid", "fluke", "flume", "flung", "flunk", "flush", "flute", "flyer", "foamy", "focal", "focus", "foggy", "foist", "folds", "folio", "folks", "folly", "foods", "foray", "force", "forge", "forgo", "forms", "forte", "forth", "forts", "forty", "forum", "found", "fours", "foxes", "foyer", "frail", "frame", "frank", "fraud", "freak", "freed", "freer", "fresh", "friar", "fried", "fries", "frill", "frisk", "fritz", "frock", "frogs", "frond", "front", "frost", "froth", "frown", "froze", "fruit", "fudge", "fuels", "fugue", "fully", "fumes", "funds", "fungi", "funky", "funny", "furor", "furry", "fussy", "fuzzy", "gaffe", "gaily", "gains", "gales", "gamer", "games", "gamma", "gamut", "gases", "gassy", "gates", "gaudy", "gauge", "gaunt", "gauze", "gavel", "gawky", "gayer", "gayly", "gazed", "gazer", "gears", "gecko", "geeky", "geese", "genie", "genre", "germs", "ghost", "ghoul", "giant", "giddy", "gifts", "gipsy", "girls", "girly", "girth", "given", "giver", "gives", "glade", "gland", "glare", "glass", "glaze", "gleam", "glean", "glide", "glint", "gloat", "globe", "gloom", "glory", "gloss", "glove", "glued", "glues", "glyph", "gnash", "gnome", "goals", "goats", "godly", "going", "golem", "golly", "gonad", "goner", "goods", "goody", "gooey", "goofy", "goose", "gorge", "gouge", "gourd", "grabs", "grace", "grade", "graft", "grail", "grain", "grand", "grant", "grape", "graph", "grasp", "grass", "grate", "grave", "gravy", "graze", "great", "greed", "greek", "green", "greet", "greys", "grief", "grill", "grime", "grimy", "grind", "grins", "gripe", "groan", "groin", "groom", "grope", "gross", "group", "grout", "grove", "growl", "grown", "grows", "gruel", "gruff", "grunt", "guard", "guava", "guess", "guest", "guide", "guild", "guile", "guilt", "guise", "gulch", "gulls", "gully", "gumbo", "gummy", "guppy", "gusto", "gusty", "gypsy", "habit", "hairs", "hairy", "halls", "halve", "hands", "handy", "hangs", "happy", "hardy", "harem", "harms", "harpy", "harry", "harsh", "haste", "hasty", "hatch", "hated", "hater", "hates", "haunt", "haute", "haven", "havoc", "hazel", "heads", "heady", "heals", "heard", "hears", "heart", "heath", "heats", "heave", "heavy", "hedge", "heels", "hefty", "heist", "helix", "hello", "helps", "hence", "herbs", "herds", "heres", "heron", "hides", "hills", "hilly", "hinge", "hippo", "hippy", "hired", "hitch", "hoard", "hobby", "hoist", "holds", "holes", "holly", "homer", "homes", "honey", "honor", "hooks", "hoped", "hopes", "horde", "horns", "horny", "horse", "hotel", "hotly", "hound", "hours", "house", "hovel", "hover", "howdy", "human", "humid", "humor", "humph", "humus", "hunch", "hunky", "hunts", "hurry", "hurts", "husky", "hussy", "hutch", "hydro", "hyena", "hymen", "hyper", "icily", "icing", "ideal", "ideas", "idiom", "idiot", "idler", "idyll", "igloo", "iliac", "image", "imbue", "impel", "imply", "inane", "inbox", "incur", "index", "india", "inept", "inert", "infer", "ingot", "inlay", "inlet", "inner", "input", "inter", "intro", "ionic", "irate", "irish", "irons", "irony", "islet", "issue", "italy", "itchy", "items", "ivory", "japan", "jaunt", "jazzy", "jeans", "jello", "jelly", "jerky", "jesus", "jetty", "jewel", "jiffy", "joins", "joint", "joist", "joker", "jokes", "jolly", "joust", "judge", "juice", "juicy", "jumbo", "jumps", "jumpy", "junta", "junto", "juror", "kappa", "karma", "kayak", "kebab", "keeps", "khaki", "kicks", "kills", "kinds", "kings", "kinky", "kiosk", "kites", "kitty", "knack", "knave", "knead", "kneed", "kneel", "knees", "knelt", "knife", "knits", "knobs", "knock", "knoll", "knots", "known", "knows", "koala", "krill", "label", "labor", "laden", "ladle", "lager", "lakes", "lambs", "lamps", "lance", "lands", "lanes", "lanky", "lapel", "lapse", "large", "larva", "lasso", "lasts", "latch", "later", "lathe", "latin", "latte", "laugh", "lawns", "layer", "leach", "leads", "leafy", "leaky", "leant", "leaps", "leapt", "learn", "lease", "leash", "least", "leave", "ledge", "leech", "leery", "lefty", "legal", "leggy", "lemon", "lemur", "lends", "leper", "level", "lever", "libel", "liege", "lifts", "light", "liked", "liken", "likes", "lilac", "limbo", "limbs", "limit", "lined", "linen", "liner", "lines", "lingo", "links", "lions", "lipid", "lists", "liter", "lithe", "lived", "liver", "lives", "livid", "llama", "loads", "loamy", "loath", "lobby", "local", "locks", "locus", "lodge", "lofty", "logic", "login", "looks", "loops", "loopy", "loose", "lords", "lorry", "loser", "loses", "louse", "lousy", "loved", "lover", "loves", "lower", "lowly", "loyal", "lucid", "lucky", "lumen", "lumps", "lumpy", "lunar", "lunch", "lunge", "lungs", "lupus", "lurch", "lurid", "lusty", "lying", "lymph", "lynch", "lyric", "macaw", "macho", "macro", "madam", "madly", "mafia", "magic", "magma", "maids", "mails", "maize", "major", "maker", "makes", "males", "mambo", "mamma", "mammy", "manga", "mange", "mango", "mangy", "mania", "manic", "manly", "manor", "maple", "march", "marks", "marry", "marsh", "masks", "mason", "masse", "match", "matey", "mauve", "maxim", "maybe", "mayor", "meals", "mealy", "means", "meant", "meats", "meaty", "mecca", "medal", "media", "medic", "meets", "melee", "melon", "melts", "mends", "mercy", "merge", "merit", "merry", "metal", "meter", "metre", "metro", "micro", "midge", "midst", "might", "miles", "milky", "mills", "mimes", "mimic", "mince", "minds", "mined", "miner", "minim", "minor", "minty", "minus", "mirth", "miser", "missy", "mists", "mixed", "mixes", "mocha", "modal", "model", "modem", "mogul", "moist", "molar", "molds", "moldy", "moles", "money", "month", "moods", "moody", "moons", "moose", "moral", "moron", "morph", "mossy", "motel", "moths", "motif", "motor", "motto", "moult", "mound", "mount", "mourn", "mouse", "mouth", "moved", "mover", "moves", "movie", "mower", "mucky", "mucus", "muddy", "mulch", "mules", "mummy", "munch", "mural", "murky", "mushy", "music", "musky", "musty", "myrrh", "myths", "nadir", "nails", "naive", "naked", "named", "names", "nanny", "nasal", "nasty", "natal", "naval", "navel", "necks", "needs", "needy", "neigh", "nerdy", "nerve", "nests", "never", "newer", "newly", "nicer", "niche", "niece", "night", "nines", "ninja", "ninny", "ninth", "noble", "nobly", "noise", "noisy", "nomad", "noone", "noose", "north", "noses", "nosey", "notch", "noted", "notes", "nouns", "novel", "nudge", "nurse", "nutty", "nylon", "nymph", "oaken", "oasis", "obese", "obeys", "occur", "ocean", "octal", "octet", "odder", "oddly", "offal", "offer", "often", "oiled", "olden", "older", "olive", "ombre", "omega", "onion", "onset", "opens", "opera", "opine", "opium", "optic", "orbit", "order", "organ", "other", "otter", "ought", "ounce", "outdo", "outer", "outgo", "ovary", "ovate", "ovens", "overt", "ovine", "ovoid", "owing", "owned", "owner", "oxide", "ozone", "packs", "paddy", "pagan", "pages", "pains", "paint", "pairs", "paler", "palms", "palsy", "panel", "panic", "pansy", "pants", "papal", "paper", "parer", "parka", "parks", "parry", "parse", "parts", "party", "pasta", "paste", "pasty", "patch", "paths", "patio", "patsy", "patty", "pause", "paved", "payee", "payer", "peace", "peach", "peaks", "pearl", "pears", "pecan", "pedal", "peels", "peeps", "penal", "pence", "penne", "penny", "perch", "peril", "perky", "pesky", "pesto", "petal", "petty", "phase", "phone", "phony", "photo", "piano", "picks", "picky", "piece", "piety", "piggy", "piled", "piles", "pills", "pilot", "pinch", "pines", "piney", "pinky", "pinto", "piper", "pipes", "pique", "pitch", "pithy", "pivot", "pixel", "pixie", "pizza", "place", "plaid", "plain", "plait", "plane", "plank", "plans", "plant", "plate", "plays", "plaza", "plead", "pleat", "plied", "plier", "plows", "pluck", "plumb", "plume", "plump", "plunk", "plush", "poems", "poesy", "poets", "point", "poise", "poked", "poker", "polar", "poles", "polka", "polyp", "ponds", "pooch", "pools", "poppy", "porch", "ports", "poser", "posit", "posse", "posts", "pouch", "pound", "pours", "pouty", "power", "prank", "prawn", "preen", "press", "price", "prick", "pride", "pried", "prime", "primo", "print", "prior", "prism", "privy", "prize", "probe", "prone", "prong", "proof", "prose", "proud", "prove", "prowl", "proxy", "prude", "prune", "psalm", "pubic", "pudgy", "puffs", "puffy", "pulls", "pulpy", "pulse", "pumps", "punch", "pupal", "pupil", "puppy", "puree", "purer", "purge", "purse", "pushy", "putty", "pygmy", "quack", "quail", "quake", "qualm", "quark", "quart", "quash", "quasi", "queen", "queer", "quell", "query", "quest", "queue", "quick", "quiet", "quill", "quilt", "quirk", "quite", "quota", "quote", "quoth", "rabbi", "rabid", "raced", "racer", "races", "racks", "radar", "radii", "radio", "rafts", "rails", "rains", "rainy", "raise", "rajah", "rally", "ralph", "ramen", "ranch", "randy", "range", "ranks", "rapid", "rarer", "raspy", "rates", "ratio", "ratty", "raven", "rayon", "razor", "reach", "react", "reads", "ready", "realm", "rearm", "rebar", "rebel", "rebus", "rebut", "recap", "recur", "recut", "reeds", "reedy", "refer", "refit", "regal", "rehab", "reign", "reins", "relax", "relay", "relic", "remit", "renal", "renew", "repay", "repel", "reply", "rerun", "reset", "resin", "rests", "retch", "retro", "retry", "reuse", "revel", "revue", "rhino", "rhyme", "rider", "rides", "ridge", "rifle", "right", "rigid", "rigor", "rings", "rinks", "rinse", "riots", "ripen", "riper", "risen", "riser", "rises", "risky", "rival", "river", "rivet", "roach", "roads", "roars", "roast", "robes", "robin", "robot", "rocks", "rocky", "rodeo", "roger", "rogue", "rolls", "roman", "roofs", "rooms", "roomy", "roost", "roots", "roped", "ropes", "roses", "rotor", "rouge", "rough", "round", "rouse", "route", "rover", "rowdy", "rower", "royal", "ruddy", "ruder", "rugby", "ruins", "ruled", "ruler", "rules", "rumba", "rumor", "rungs", "rupee", "rural", "rusty", "sacks", "sadly", "safer", "sails", "saint", "salad", "sales", "sally", "salon", "salsa", "salty", "salve", "salvo", "sands", "sandy", "saner", "sappy", "sassy", "satin", "satyr", "sauce", "saucy", "sauna", "saute", "saved", "saves", "savor", "savoy", "savvy", "scald", "scale", "scalp", "scaly", "scamp", "scant", "scare", "scarf", "scary", "scene", "scent", "scion", "scoff", "scold", "scone", "scoop", "scope", "score", "scorn", "scour", "scout", "scowl", "scram", "scrap", "scree", "screw", "scrub", "scrum", "scuba", "seals", "seats", "sedan", "seeds", "seedy", "seems", "segue", "seize", "sells", "semen", "sends", "sense", "sepia", "serif", "serum", "serve", "setup", "seven", "sever", "sewed", "sewer", "shack", "shade", "shady", "shaft", "shake", "shaky", "shale", "shall", "shalt", "shame", "shank", "shape", "shard", "share", "shark", "sharp", "shave", "shawl", "shear", "sheds", "sheen", "sheep", "sheer", "sheet", "sheik", "shelf", "shell", "shied", "shift", "shine", "shiny", "ships", "shire", "shirk", "shirt", "shoal", "shock", "shoes", "shone", "shook", "shoot", "shops", "shore", "shorn", "short", "shots", "shout", "shove", "shown", "shows", "showy", "shrew", "shrub", "shrug", "shuck", "shunt", "shush", "shuts", "shyly", "sides", "siege", "sieve", "sight", "sigma", "signs", "silks", "silky", "silly", "since", "sinew", "singe", "sings", "sinks", "siren", "sirup", "sissy", "sixes", "sixth", "sixty", "sized", "sizes", "skate", "skied", "skier", "skies", "skiff", "skill", "skimp", "skins", "skips", "skirt", "skulk", "skull", "skunk", "slack", "slain", "slang", "slant", "slash", "slate", "slave", "sleek", "sleep", "sleet", "slept", "slice", "slick", "slide", "slime", "slimy", "sling", "slink", "slips", "sloop", "slope", "slosh", "sloth", "slows", "slump", "slung", "slunk", "slurp", "slush", "slyly", "smack", "small", "smart", "smash", "smear", "smell", "smelt", "smile", "smirk", "smite", "smith", "smock", "smoke", "smoky", "smote", "snack", "snail", "snake", "snaky", "snaps", "snare", "snarl", "sneak", "sneer", "snide", "sniff", "snipe", "snoop", "snore", "snort", "snout", "snows", "snowy", "snuck", "snuff", "soapy", "sober", "socks", "soggy", "soils", "solar", "solid", "solve", "sonar", "songs", "sonic", "sooth", "sooty", "sorry", "sorts", "sound", "south", "sower", "space", "spade", "spain", "spank", "spare", "spark", "spasm", "spawn", "speak", "spear", "speck", "speed", "spell", "spelt", "spend", "spent", "sperm", "spice", "spicy", "spied", "spiel", "spike", "spiky", "spill", "spilt", "spine", "spins", "spiny", "spire", "spite", "splat", "split", "spoil", "spoke", "spoof", "spook", "spool", "spoon", "spore", "sport", "spots", "spout", "spray", "spree", "sprig", "spunk", "spurn", "spurt", "squad", "squat", "squib", "stack", "staff", "stage", "staid", "stain", "stair", "stake", "stale", "stalk", "stall", "stamp", "stand", "stank", "stare", "stark", "stars", "start", "stash", "state", "stave", "stays", "stead", "steak", "steal", "steam", "steed", "steel", "steep", "steer", "stein", "stems", "steps", "stern", "stick", "stiff", "still", "stilt", "sting", "stink", "stint", "stirs", "stock", "stoic", "stoke", "stole", "stomp", "stone", "stony", "stood", "stool", "stoop", "stops", "store", "stork", "storm", "story", "stout", "stove", "strap", "straw", "stray", "strip", "strut", "stuck", "study", "stuff", "stump", "stung", "stunk", "stuns", "stunt", "style", "suave", "sugar", "suing", "suite", "suits", "sulky", "sully", "sumac", "sunny", "super", "surer", "surge", "surly", "sushi", "swami", "swamp", "swarm", "swash", "swath", "swear", "sweat", "sweep", "sweet", "swell", "swept", "swift", "swill", "swims", "swine", "swing", "swirl", "swish", "swiss", "swoon", "swoop", "sword", "swore", "sworn", "swung", "synod", "syrup", "tabby", "table", "taboo", "tacit", "tacky", "taffy", "tails", "taint", "taken", "taker", "takes", "tales", "talks", "tally", "talon", "tamer", "tango", "tangy", "tanks", "taper", "tapir", "tardy", "tarot", "tarts", "tasks", "taste", "tasty", "tatty", "taunt", "tawny", "taxes", "teach", "teams", "tears", "teary", "tease", "teddy", "teeth", "tells", "tempo", "tends", "tenet", "tenor", "tense", "tenth", "tents", "tepee", "tepid", "terms", "terra", "terse", "tests", "testy", "thank", "theft", "their", "theme", "there", "these", "theta", "thick", "thief", "thigh", "thing", "think", "third", "thong", "thorn", "those", "three", "threw", "throb", "throw", "thrum", "thumb", "thump", "thyme", "tiara", "tibia", "tidal", "tides", "tiger", "tight", "tilde", "tiles", "timer", "times", "timid", "tipsy", "tired", "tires", "titan", "tithe", "title", "toads", "toast", "today", "toddy", "token", "tonal", "tones", "tonga", "tonic", "tools", "tooth", "topaz", "topic", "torch", "torso", "torus", "total", "totem", "touch", "tough", "towel", "tower", "towns", "toxic", "toxin", "trace", "track", "tract", "trade", "trail", "train", "trait", "tramp", "traps", "trash", "trawl", "tread", "treat", "trees", "trend", "triad", "trial", "tribe", "trice", "trick", "tried", "tries", "tripe", "trips", "trite", "troll", "troop", "trope", "trout", "trove", "truce", "truck", "truer", "truly", "trump", "trunk", "truss", "trust", "truth", "tryst", "tubal", "tuber", "tubes", "tulip", "tulle", "tummy", "tumor", "tunes", "tunic", "turbo", "turns", "tusks", "tutor", "twang", "tweak", "tweed", "tweet", "twice", "twigs", "twine", "twins", "twirl", "twist", "twixt", "tying", "types", "tyres", "udder", "ulcer", "ultra", "umbra", "uncle", "uncut", "under", "undid", "undue", "unfed", "unfit", "unify", "union", "unite", "units", "unity", "unlit", "unmet", "unset", "untie", "until", "unwed", "unzip", "upper", "upset", "urban", "urged", "urine", "usage", "usher", "using", "usual", "usurp", "utile", "utter", "vague", "valet", "valid", "valor", "value", "valve", "vapid", "vapor", "vases", "vault", "vaunt", "vegan", "veins", "venom", "venue", "venus", "verbs", "verge", "verse", "verso", "verve", "vicar", "video", "views", "vigil", "vigor", "villa", "vines", "vinyl", "viola", "viper", "viral", "virus", "visit", "visor", "vista", "vital", "vivid", "vixen", "vocal", "vodka", "vogue", "voice", "voila", "vomit", "voted", "voter", "votes", "vouch", "vowel", "vying", "wacky", "waded", "wafer", "wager", "wages", "wagon", "waist", "waits", "waive", "wakes", "walks", "walls", "waltz", "wants", "warms", "warty", "waste", "watch", "water", "waved", "waver", "waves", "waxen", "wears", "weary", "weave", "wedge", "weeds", "weedy", "weeks", "weigh", "weird", "welch", "wells", "welsh", "wench", "whack", "whale", "wharf", "wheat", "wheel", "whelp", "where", "which", "whiff", "while", "whine", "whiny", "whirl", "whisk", "white", "whole", "whoop", "whose", "widen", "wider", "widow", "width", "wield", "wight", "willy", "wimpy", "wince", "winch", "winds", "windy", "wings", "wiped", "wipes", "wired", "wires", "wiser", "wispy", "witch", "witty", "wives", "woken", "woman", "women", "woods", "woody", "wooer", "wooly", "woozy", "words", "wordy", "works", "world", "worms", "worry", "worse", "worst", "worth", "would", "wound", "woven", "wrack", "wraps", "wrath", "wreak", "wreck", "wrest", "wring", "wrist", "write", "wrong", "wrote", "wrung", "wryly", "xrays", "yacht", "yards", "yawns", "yearn", "years", "yeast", "yield", "yolks", "young", "yours", "youth", "yoyos", "zebra", "zesty", "zonal"},ResourceData[ResourceObject["Wordle Word List"]]]],
	i,n,keys={},word="",letters={},correct={},history={},
	green={},gray={},yellow={},colored={},def={},len=True,realword=True,
	temp=ConstantArray["",5],tempgreen={},tempyellow={},
	(*dgy=RGBColor["#3a3a3c"],*)dgy=GrayLevel[0.375],dyw=RGBColor["#eab308"],dgn=RGBColor["#63aa55"],lgy=GrayLevel[0.675],
	md,fr=None,opts={}
	},

	n=RandomInteger[{1,Length[list]}];
	word=ToLowerCase[list[[n]]];
	letters=Characters[word];
	
	def=WordDefinition[word];
	If[Length[def]>30,
		opts={Scrollbars->{False,True},ImageSizeAction->"Scrollable",ScrollPosition->{0,0},ImageSize->{Automatic,500}},
		opts={}
	];
	md=Inactive[MessageDialog[Pane[
		Column[{
			Style["Definition(s):", FontWeight -> Bold],
			Column[Table["     " <> ToString[i] <> ". " <> def[[i]],{i,1,Length[def]}]]
		}]
	, opts]]];
	
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
			EventHandler[
				InputField[Dynamic[x],FieldHint->"Enter your guess",FieldSize->{20,1},BaselinePosition->Scaled[0],BaseStyle->{FontSize->16,FontFamily->"Arial"}],
				{
					"ReturnKeyDown":>{
						If[StringLength[ToString[x]]==5,len=True,(len=False;MessageDialog["Guesses must have exactly five letters. Your guess of \"" <> ToLowerCase[ToString[x]] <> "\" has " <> ToString[StringLength[ToString[x]]] <> " letters."])];
						If[MemberQ[list2,ToLowerCase[ToString[x]]],realword=True,(realword=False;If[len==True,MessageDialog["Guesses must consist of valid dictionary words. Your guess of \"" <> ToLowerCase[ToString[x]] <> "\" is not in the dictionary."]])];
						If[done==False&&guesses>0&&len==True&&realword==True,main[ToString[x],guesses--]];
					}
				}
			],
			(* InputField[Dynamic[x],FieldHint\[Rule]"Enter your guess",FieldSize->{20,1},BaselinePosition->Scaled[0],BaseStyle->{FontSize->16,FontFamily->"Arial"}],*),
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
			letters=Characters[word];
			def=WordDefinition[word];
			If[Length[def]>30,
				opts={Scrollbars->{False,True},ImageSizeAction->"Scrollable",ScrollPosition->{0,0},ImageSize->{Automatic,500}},
				opts={}
			];
			md=Inactive[MessageDialog[Pane[
				Column[{
					Style["Definition(s):", FontWeight -> Bold],
					Column[Table["     " <> ToString[i] <> ". " <> def[[i]],{i,1,Length[def]}]]
				}]
			, opts]]];
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


(* ::Subsection:: *)
(*v 1.1.0*)


(* ::Item:: *)
(*The \!\( *)
(*TagBox[*)
(*ButtonBox[*)
(*StyleBox["InputField", "SymbolsRefLink",*)
(*ShowStringCharacters->True,*)
(*FontFamily->"Source Sans Pro"],*)
(*BaseStyle->{"Link"},*)
(*ButtonData->"paclet:ref/InputField",*)
(*ContentPadding->False],*)
(*MouseAppearanceTag["LinkHand"]]\) was wrapped in an \!\( *)
(*TagBox[*)
(*ButtonBox[*)
(*StyleBox["EventHandler", "SymbolsRefLink",*)
(*ShowStringCharacters->True,*)
(*FontFamily->"Source Sans Pro"],*)
(*BaseStyle->{"Link"},*)
(*ButtonData->"paclet:ref/EventHandler",*)
(*ContentPadding->False],*)
(*MouseAppearanceTag["LinkHand"]]\) so that guesses can be submitted using the \!\( *)
(*TagBox["\[EnterKey]",*)
(*"ForceRasterization"]\) key.*)


(* ::Subitem:: *)
(*This one's for you, Bob!*)


(* ::Item:: *)
(*Scrolling behavior (via \!\( *)
(*TagBox[*)
(*ButtonBox[*)
(*StyleBox["Pane", "SymbolsRefLink",*)
(*ShowStringCharacters->True,*)
(*FontFamily->"Source Sans Pro"],*)
(*BaseStyle->{"Link"},*)
(*ButtonData->"paclet:ref/Pane",*)
(*ContentPadding->False],*)
(*MouseAppearanceTag["LinkHand"]]\) and options) has been (conditionally) added to the "Definitions" \!\( *)
(*TagBox[*)
(*ButtonBox[*)
(*StyleBox["MessageDialog", "SymbolsRefLink",*)
(*ShowStringCharacters->True,*)
(*FontFamily->"Source Sans Pro"],*)
(*BaseStyle->{"Link"},*)
(*ButtonData->"paclet:ref/MessageDialog",*)
(*ContentPadding->False],*)
(*MouseAppearanceTag["LinkHand"]]\) in the event that words have a large number of definitions (e.g. "clear").*)


(* ::Item:: *)
(*A modified wordlist was implemented. This contains pieces of the v1.0.0 wordlist combined with some of the words in the official Wordle list.*)


(* ::Subitem:: *)
(*Special care was implemented so that contractions are excluded and obscenities are largely removed.*)


(* ::Item:: *)
(*Support was added for guesses containing uppercase letters. The "Scope" subsection was updated accordingly.*)


(* ::Item:: *)
(*The D&O section was modified, and some of the verbiage in "Possible Issues" was updated.*)


(* ::Subsection:: *)
(*v 1.0.0*)


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
(*allowing users to choose from different word lists (the Wordle one, WordList and maybe others)*)


(* ::Subitem:: *)
(*adjusting the number of initial guesses*)


(* ::Subitem:: *)
(*specifying whether guesses of length \!\( *)
(*TagBox[*)
(*RowBox[{*)
(*StyleBox["len", "TI"], "!=", "5"}],*)
(*"ForceRasterization"]\) are allowed*)


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
