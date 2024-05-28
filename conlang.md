    I tried and failed last year to write an English parser that generated meaning-preserving translations in Toki Pona.
    There are a couple of aspects of Toki Pona that make it difficult for computers: most words have several definitions
    spanning multiple parts of speech, and there are often multiple tactics for translations of words that have similar
    results.
    
    "Turing-Completeness", with regards to programming languages, means that one is capable of manipulating, storing, and
    reading data. A barebones example is the language "Brainfuck," with only six commands. In practice, most programming
    languages are Turing-Complete, and as such, all are capable and incapable of similar things. Natural Language can be
    used to represent all known human concepts, with occasional use of new vocabulary or circumlocution. One of the 
    simpler Chinese words that gets floated as an example with no English equivalent is "吗 (ma)". This is a sentence
    particle, which when placed at the end of a sentence changes it to a question, asking whether what preceded it was 
    true or false. For example, "You're happy 吗" = "Are you happy (y/n)"? There isn't an English word that carries this
    meaning, but there is an English phrase (": is that so?"), as well as other techniques for translating yes/no questions
    to English. In general, natural languages can all express roughly equivalent semantic concepts. This is somewhat like 
    Turing-Completeness: I'll call it "Chomsky-Completeness," because I am an ardent leftist. 
    
    Def. "Chomsky-Complete" - of a language, capable of representing arbitrary semantic concepts, only using loanwords to 
    represent previously unknown materials or animals. 
    
    What follows is an attempt at a modular, lightweight constructed language without these traits, specifically intended
    for algorithmic analysis of semantics. "Circumlocution" is the use of many words when few can supply similar semantic
    meaning, ex. "Evasive with truth" -> "Lying". This language relies on this tactic to express complex words like 
    "electromagnetic" or "questionnaire". There are 32 characters in the alphabet, including spaces. 
    
    -----------------------------------------------------------------------------------------------
    
    A simplified "Generative Grammar" of English looks like this. Brackets around a term mean that it is optional.
    
    <S>  := Sentence
            ex. "I'm feeling good today."
    <NP> := Noun Phrase
            ex. "The hairy dog", "All chimpanzees", "I"
    <VP> := Verb Phrase
            ex. "walked slowly", "pulled the casserole out of the oven"
    <CP> := Context Phrase
            ex. "into a life", "and Fred"
    
    <S> := <NP> <VP>
    <NP> := [Det] [Adj] Noun [PP]
    <VP> := Verb [NP] [PP] [Adv]
    <CP> := Prep NP [Adv]
    
    A Noun is a person, place, thing, idea, or phenomenon. A Prep is either a preposition or conjunction, capable of 
    linking two NPs, or a VP to an NP. "Det" is short for "Determiner".
    
    In this language, there are 18 parts of speech. 
    
    MATERIAL   - noun or adjective, like "glass", "metal", "oil".
    ANIMATE    - human or visible non-sessile creature.
    OBJECT     - common shape or format.
    DEVICE     - specialized object used by humans for a particular purpose.
    PLACE      - particular position, event, or subset of the universe.
    CONCEPT    - invented construct that cannot be seen. 
    PHENOMENON - common condition that was discovered, not created. 
    MEASURE    - countable quantity. 
    ORGAN      - part of an animate. 
    
    DETERMINER - quantitative or relative designator of members of a class. 
    
    CONJUNCTION - word connecting two clauses. 
    
    TRAIT   - Adjective with antonym, describing texture, structure, or lifestyle.
    QUALITY - Adjective without antonym, describing color, taste, or emotion. 
    
    ADVERB  - Word modifying an adjective.
    
    MODAL - Verb that precedes another verb, like "do", "have", or "should". 
    OBJECTIVE - Verb manipulating a noun. 
    CORPOREAL - Action accomplished by using only one's body. 
    
    INTERJECTION - Social nicety or emotional outburst. 
    
    CIRCUMLOC
    
    x="th"
    c="sh"
    q="ch"
    r="er"
    
    A="ay"
    E="ee"
    I="eye"
    O="owe"
    U="you"
    a="ah"
    e="eh"
    i="ih"
    o="aw"
    u="uh"
    
    
    
    MATERIAL 
    
      efem      - one of the following gaseous materials.
        Ar      - the gas present in the atmosphere, mainly nitrogen
        cadO    - vacuum, shadow; absence of light, air, or matter
        wexr    - state of atmosphere regarding heat, dryness, wind, rain, etc
        
        oksEjin - oxygen, air as or before it is inhaled by a living organism
        Elyum   - helium
        Idro    - hydrogen
        fUm     - carbon dioxide, monoxide - air exhaled by organism or machine.
        klrIn   - chlorine
        mexAn   - methane, farts
    
        enrjE   - energy, calorie, force, thrust; power to provide heat or movement
        bUm     - violent explosion or expansion of energy 
        fIr     - bright warm light produced through combustion
        lIt     - the natural agent that makes things visible
        magne   - force that pulls magnetized objects together
        pEka    - electric energy produced by charge, magnets, or other  
        radEO   - GPS, radio, cell service, 5G; inaudible waves through which data is sent
        smOk    - visible gaseous emission of burning or boiling substance
        
        odEO    - vibrations that can be heard, which can convey speech, music, etc.
        Odr     - a distinctive smell or scent
        glO     - euphoria, bliss, joy; positive physical sensation
        pAn     - pain, hurt, suffering; physical suffering or discomfort caused by illness or injury
        
      likwid   - one of the following liquid materials.
        asid   - corrosive, sour liquid with low pH. 
        oyul   - oil, wax; a greasy combustible liquid burned to generate heat or power or minimizes friction; insoluble in water
        likr   - alcoholic flammable intoxicant liquid produced by sugar fermentation
        mrkrE  - mercury, quicksilver
        lavu   - magma, lava; molten rock
        sOp    - ammonia, soap, bleach; alkaline liquid used for cleaning
        watr   - transparent, clear liquid composed primarily of H2O 
          rivr   - river, stream, creek; stream of water continuously flowing
          lAk    - lake, pond, ocean; pool of still water
        
        dArI    - opaque white fluid secreted by female mammal 
        eg      - animal reproductive material
        pis     - piss
        blud    - red liquid that circulates oxygen in vertebrates
        venim   - arsenic, poison; substance capable of causing illness or death to a living organism.
        glU     - an adhesive liquid used for sticking objects together
        Enk     - a colored fluid used for writing or drawing
        slIm    - sauce, toothpaste, sticky-tack, putty, play-dough, oobleck - non-newtonian fluid
        kofE    - coffee, tea, hot cocoa; dark, bitter beverage made by soaking plant matter in hot water
        sirup   - sweet sticky substance primarily consisting of sugar
        sUp     - soup -  edible flavored liquid
    
      pawdr     - one of the following powdered materials
        salt    - sodium, salt; sodium-based crystal from the sea used in seasoning food. 
        sand    - sand
        spIs    - black pepper, chili; seasoning bearing capsicum or pepper
        klOv    - cinnamon, nutmeg, vanilla; spice used in baking
        mint    - basil, cilantro, fennel; flavorful herb
        mustrd  - mustard, wasabi; pungent, bitter condiment
        kUmin   - savory, earthy spice
        drug    - weed, LSD, opium; non-alcoholic psychotropic substance
        yEst    - spore, mold, pollen, mycelium; minute reproductive unit of plant or fungus
        legUm   - bean, soy, peas; edible pod of seeds from leguminous plant.
        sEd     - seed, nut; hard reproductive unit of a plant.
        grAn    - rice, flour, pasta; grain made up of many small parts.
        
      solid     - one of the following solid materials
        Is      - ice, snow, frozen liquid
        gold    - gold 
        silvr   - silver
        tasyum  - potassium
        slikon - silicon
        krben  - carbon
        sulfr  - sulfur, brimstone, vitriol, gunpowder; explosive natural material
        plastik - rubber, plastic, latex; tough elastic hydrocarbon.
        linen   - silk, cloth; woven soft fabric used for clothing, carpet, or rags.
        lumbr   - hard, fibrous material inside of trees.
        glas    - transparent, brittle substance used in windows, cups, and spectacles.
        nUk     - radioactive material
        jim     - gold, diamond, precious stone
        asfalt  - brick, concrete, gravel; powdered rock used for roads 
        kOl     - lead, coal, chalk, porcelain; fragile rock that can leave pigment behind on other rocks.
        magnet - magnet
        stOn    - rock, stone; cohesive mineral mass
        metul   - shiny grey conductive sometimes-magnetic material 
        bronz   - copper, brass; shiny brown metal with high thermal conductivity
        cit     - feces
        
      sesIl   - one of the following living sessile materials
        plant - sessile organism using photosynthesis and reproducing asexually through pollen or seeds. 
        rUt   - root, ginger, turmeric - underground portion of plant 
        lEf   - flat, pliable outgrowth of plant where photosynthesis occurs.
        flawr - petalled, colorful part of plant bearing reproductive matter.
        vIn   - supporting stalk of a plant.
        trE   - wooden, large plant that undergoes a yearly cycle.
        hedj  - grass, shrub, low plant that can be walked through.
        fungI - fungus; visible spore-producing saprophytic filamentous organism
        umEbu - bacteria, plankton, tardigrade; single-celled organism capable of teamwork or spreading disease
        kwIn  - virus, flu, prion, cancer; form of information that multiplies uncontrollably, often leading to sickness
        mos   - moss, algae; plant or fungus with no stalk
        
        
        mAngO - apple, peach, mango, avocado; reproductive unit of plant with inedible core
        berI  - tomato, berry; small, pulpy fruit
        lIm   - citrus, orange, lemon, pineapple; acidic fruit
        melun - banana, melon, cucumber - oblong fruit with edible center
        
        tAtr  - potato, yam; starchy tuberous root vegetable
        unyun - garlic, onion; pungent bulbous plant bearing allium 
        trnup - carrot, beet, radish; mild-flavored root vegetable
        pepr  - gourd, squash, bell pepper; vegetable with a handle on top.
        kabaj - lettuce, brocolli, spinach; mild green leafy vegetable of Brassica genus.
        
      bodE - body, self
        mEt - dead animal parts used for different purpose 
        lim - arm, leg, wing, fin 
        dijit - finger, toe, tongue 
        hand - hand, foot, hoof 
        nOz - nose, horn 
        belE - chest, abdomen, bottom half of body 
        brest - thorax, breasts, top half of body 
        tAl - back, butt, tail 
        fAs - face, head
        mawx - mouth, beak, sucker, lips
        lung - lung, gill 
        fat - fat, cartilage
        gland - esoteric organ, tissue, kidney
        bOn - bone, claw, tooth, skull
        brAn - brain, nerves, rational sense
        gut - heart, vessels, emotional sense 
        skin - skin, scales
        stumak - stomach, intestine
        bron  - muscles, physical prowess
        fuz  - hair, fur, feathers 
        luins - sexual organ 
        OkO - eye, camera, view, sight
        nek - chin, neck, throat
        fOn - ear, microphone
        wUnd - wound, injury  
    
        
    koncus - any organism of Animalia or equivalent extraterrestrial kingdom that was at one point alive.
    
      bug      - any insect, arthropod, or germ.
        ant    - ant, grasshopper, mantis, cricket; terrestrial insect
        bEn    - bee, wasp; stinging, flying, insect.
        nat    - fly, gnat, flea; pesky, buzzing, parasitic insect 
        wrm    - worm, caterpillar, slug - limbless burrowing bug which eats soil.
        lOkust - beetle, roach, roly-poly, cicada; blocky creature resembling a beetle.
        spIdr  - eight-legged arthropod that spins webs and poisons prey, scorpion.
        skEtO  - mosquito, tick; bug which sucks human blood and trasmits disease.
        mox    - butterfly, dragonfly; big-winged, harmless insect 
        molusk - clam, snail, oyster; shelled aquatic invertebrate with soft body
        krab   - shrimp, crawfish, crab; aquatic crustascean with many legs
    
      akesE - reptile, amphibian; scaly or slimy creature.
        snAk  - snake; long limbless creature with no eyelids and large jaws.
        lizrd - lizard, salamander, croc, dino; akesIn creature with four legs, tail, and eyelids.
        trtil - shelled peaceful slow creature
        tOd   - frog, toad; large-mouthed leaping amphibian with no tail
        fic   - fish; limbless cold-blooded vertebrate with gills and fins that lives in water.
        skwid - squid, octopus; mant-tentacled cephelopod.
        gU    - jellyfish; gelatinous, very simplistic aquatic animal.
    
      AvEin     - animal with beak, wings, and feathers.
        brd     - robin, mockingjay, songbird; small flying migratory bird
        qikin   - chicken, turkey; domesticated flightless bird farmed for its meat
        pAngwin - flamingo, penguin; large flightless bird that it is immoral to eat.
        duk     - duck, goose; aquatic bird
        awl     - large bird that hunts vertebrates with sharp talons.
    
      mamul     - warm-blooded mammal that produces milk
        fElIn   - cat, tiger, hyena, feline; lithe mammal with whiskers, sensitive hearing, retractible claws and gritty tongue
        cAnIn   - dog, fox, wolf, canine; long-muzzled mammal with upright ears and strong teeth.
        kamel   - horse, camel; ridable mammal
        bOvIn   - cow, bison; large domesticated mammal which chews its cud.
        wAl     - whale, elephant, seal; large hairless grey mammal
        swIn    - deer, goat, sheep, pig; medium-sized non-ridable hoofed mammal 
        rakUn   - otter, badger, skunk; small-medium solitary omnivorous nocturnal smelly mustelid
        lago    - squirrel, rabbit, beaver; defensive small land mammal 
        crU     - rat, mouse, bat, rodent; gnawing small mammal generally regarded as a pest
        simEan  - monkey, ape, primate; tree dwelling primate with complex brains, depth perception, and grasping hands.
        bAr     - bear, sloth;  large heavy omnivorous mammal of Ursidae
    
      hUmin    - human, guy, person, someone; a human
        bAbE   - baby, infant; human under four years of age
        spawn  - child, son, daughter; a younger human relative
        famlE  - family; the set of individuals one is related to, by marriage, blood, or prolongued proximity.
        siblin - sibling; a relative within 20 years of age.
        ansetr - grandparent; a relative at least two generations older.
        fren   - friend; an organism that one enjoys but does not love.
        pet    - domesticated animal for companionship
        eksprt - expert; person with exceptionally extensive knowledge on a particular topic.
        gest   - guest; a person invited to visit one's place.
        I      - I; the speaker
        U      - you; the person being spoken to 
        E      - he,she,it; the last object referenced
    
      grUp  - group, organization; multiple organisms working toward a common goal.
        us  - we; the group, including the speaker
        yal - y'all; the group, including the spoken to but not the speaker
        xA  - they; an external group 
    
      monstr   - hypothetical creature
        spErit - nymph, angel, minor god; hypothetical representative of phenomena
        AlEin  - alien; extraterrestrial life form, unknown creature
        elf    - fairy, gnome; small humanoid trickster capable of magic
        gOst   - ghost; force guided by particular once-living organism
        rObot  - robot, golem, tulpa; organism created artificially
        
      jehOva - monotheistic god of Abrahamic tradition
    
    
    
    
    
      objekt    - thing; any inanimate object 
        srfis   - floor, roof, board, tray; thin flat surface on which objects may be placed
        cel     - wall, edge, shell, cover; exterior layer or outermost visible portion of an object.
        krak    - hole, gap, orifice, door; empty space within solid object.
        mAtrks  - square, card, page, paper; square-shaped object, usually used for writing or drawing
        cord    - hair, thread, spiral, arc; long, thin, flexible thing.
        srkil   - circle, wheel, ball, clock; circular, spherical, or amorphously round object
        set     - set, box, bottle, battery; unordered collection of objects
        lIn     - line, pen, rail, stack; rigid long thing
        netwrk  - weave, net, web, graph; complex structure with many intersecting lines
        et      - atom, point, dot, drop; elementary unit or particle
        it      - it; the last object referenced
        lEnk    - cross, hinge, elbow, connection; where two objects connect.
        junk    - waste, junk, trash, excrement; unwanted byproducts
        lUpik  - loop, wave, sine, modulus; a recurring cycle
        instins - example, specimen; an instance of the given class 
        dEtAl   - part, section, aspect, layer; particular component of an object or system
        miks    - compound, fusion, combination, mixture;  a mix containing more than one material or influence
        trAs    - residue, trace, souvenir; evidence of previous events
        trap    - trap, liability, risk; a surprisingly bad or dangerous thing
        kopE    - twin, facsimile, duplicate, copy; object very similar looking to another proximate object.
    
        data    - data, information, genetics, media; a quantity of information with word count exceeding 2^19
        album   - album, book, website, guide; a quantity of information with word count below 2^19 and exceeding 2^14
        song    - statement, article, essay, story; a quantity of information with word count below 2^14 and exceeding 2^5
        vrs     - sentence, verse, phrase, note; a quantity of information with word count below 2^5
        wrd     - word, name; a single distinct meaningful element of language with semantic meaning
        mark    - char, mark, letter, flag; a single character of an alphabet, without semantic meaning
        
      macEn   - machine, computer, TV, phone; mechanical apparatus with several parts.
        gun     - match, gun, geyser, bomb; device capable of rapid expulsion of material due to pressure or combustion
        switq   - switch, brake, valve, lever; device capable of maintaining one of two states.
        spAd    - plow, hoe, shovel; object used for manipulating dirt
        nAl     - arrow, needle, nail, key; long skinny object with sharp tip used for breaching solids.
        drum    - instrument piano, guitar, horn; device used for playing music 
        lok     - lock, knot, puzzle; difficult but not impossible to breach device
        nIf     - knife, scissors, sword, axe; totally sharp object used for cutting
        medisin - medicine, vaccine, pill, anesthesia; substance used for treatment or prevention of illness or wounds
        glitq   - error, mistake, paradox, contradiction; a small inconsistency 
        frAtaj  - boat, freight train, big-rig; machine for transporting materials long distances
        trAnaj  - bus, metro, airplane; machine that transports many unrelated humans 
        vEklaj  - car, bicycle, glider; privately-owned machine that transports few humans
        
        vidEO   - video, film, projection; a flat moving image 
        map     - GPS, map, compass, dictionary; a navigational aid that supplies information regarding complex structures
        refrins - reference, sign, variable; an object that represents something beyond itself
        tiket   - form, verification, stamp, patent; a document verifying one's authenticity in some respect
        
        filtr   - comb, sponge, brush; narrow-toothed or porous object used for cleansing or filtering.
        sOfa    - seat, bed, couch, chair; place where one may sit or lie down.
    
        pI      - sandwich, dumpling, pizza, taco; bread with filling as a meal 
        kAk     - cake, cookie, pie; sweet pastry
        bred    - bread, toast, tortilla, naan; baked wet yeasty flour
        mEl     - meal, breakfast, lunch, dinner; edible ingredients prepared to be eaten in one sitting
        
      plAs     - place, address, index, level; a particular position or point in space.
        bAy    - bay, shore, harbor, beach; land near a sea or ocean
        bildin - building, structure, station; roofed structure in which no one lives
        hOm    - house, home; place where someone lives 
        stAj   - stage, theatre; place where performance occurs
        ofis   - office, workplace; place where humans work
        dOjO   - school, college, dojo, academy; place where humans practice or acquire skills
        libri  - library, museum; place where information is stored
        zOn    - room; a part or division of a building enclosed by walls, floor, and ceiling.
        cop    - shop, market, bazaar; place where goods are publicly presented and sold
        status - context, environment; all circumstances that form the background to something
        sitE   - city, town; community of humans living proximately
        cAv    - cave, mine, canyon, underground; accessible area beneath the ground
        Ilend  - island; a piece of land surrounded by water
        srkus  - circus, festival, fair, party; place of mass celebration or socialization
        klub   - park, playground, nature preserve; place intentionally kept underdeveloped for recreation and conservation
        dezrt  - desert, tundra; area with very little foliage and rainfall.
        il     - hill, mountain, bump, protuberance; place that extends above the ground
        bax    - bath, puddle, pool, tub; place where still water collects temporarily
        land   - farm, garden; place where humans cultivate natural resources
        fEld   - field, plain, plateau, range; grassy, flat place
        skI    - sky, space; any place in the universe more than 50 feet above earth's surface
        pax    - path, route, road, bridge; a way or track laid down for travel
        planet - star, planet, satellite, comet; small extraterrestrial object visible from earth
        lUn    - moon; the moon 
        sol    - sun; the sun 
        event  - event, circumstance, situation; a particular time and place. 
        
        
        
        
        
        
      IdEad   - idea, concept; a thought or suggestion
    
        help     - debt, loan, assistance; work done for another person or organization
        biznis   - business, industry; large-scale economic activity
        munE     - money, tax, reward, prize; object used as payment for work or achievment
        aset     - asset, property, investment, purchase; thing that someone has.
        pawr     - power, control, authority; the ability to control other's lives.
        kultr    - culture, nation, country, society; large group of people united by common lifestyle.
        onr      - honor, reputation; one's general impression upon their community.
        mimrI    - knowledge, memory, history; recollection of past events.
        pacun    - passion, interest, engagement; enthusiasm for a given topic.    
        xeft     - theft, seizure; nonconsensual taking of someone else's property
        lojik    - study, science, math; logical or experimental reasoning.
        mexud    - route, process, algorithm; way of doing something.
        kwErE    - question, problem, doubt; a statement of an unknown that requests further information.
        xErE     - religion, belief, thought, opinion; a proposed, unproven solution to a query.
        law      - rule, law, axiom; a theory treated as fact due to its demonstrable effectiveness.
        jrnil    - news, record, press, report; a summary of events that is presented as truth.
        skeletun - structure, syntax; the underlying organizational structure of a system.    
        kaz      - proof, reason; the cause or explanation of a phenomenon.
        ufekt    - effect, result, answer; the emergent behavior of some initial condition.
        gAm      - sport, game, competition; a controlled contest of mind or body.
        dezIn    - art, design; an intentional arrangement for aesthetic purposes.
        mUsik    - rhythm, music; an artistic arrangement of sounds.
        jOk      - humor, laugh, comedy; humorous incongruence that leads to laughter.
        disEt    - lie, trick, cap, BS; untruthful statement, often to serve one's own needs.
        fantusE  - dream, fiction, ambition; a personal fantasy that helps to understand or guide.
        stIl     - vibe, style, texture, feeling; the general way that something is perceived due to intention.
        flAvr    - taste, odor; the way something is perceived by its nature.
        baluns   - balance, order, harmony; an even and satisfying arrangement.
        kAos     - chaos, anarchy, randomness; a jagged and uneven arrangement.
        los      - loss, erosion, entropy; the absence or process of removing something.
        tindinsE - tendency, habit, routine; a regular, predictable event.
        trafik   - traffic, congestion; excessive accumulation of objects leading to slowdown.
        promis   - guarantee, promise; a statement of future work or results.
        frikcun  - friction, controversy; contrasting goals leading to disagreement.
        prAz     - praise, encouragement; positive feedback so that recipient will continue current behavior.
        pitE     - pity, sympathy, grief; reciprocal negative feelings due to external negativity.
        prOtest  - reaction, resistance; attempts to halt or reverse external stimulus.
        advIs    - advice, recommendation; statements intended for an external change in behavior.
        stres    - pressure, strain; intense stress exterted on something.
        sekritE  - protection, armor; apparatus that affords safety in dangerous situations.
        majik   - magic, witchcraft; the power to influence reality by unknown means
        tIp      - type, category; a subset of objects that share some trait.
        akcun    - verb, action; something that can be done.
        trAt     - adjective, attribute; aspect of an object.
        
        
        
        
      numbr - amount, number; enumerative numbers, integers.
        lEngx - length, width, depth, inch, foot mile; notion of distance; real numbers.
        kost  - cost, value, price; monetary worth.
        slOp - direction, derivative, gradient; the rate at which something changes.
        sIz - mass, area, gradient, determinent, integral
        agin - again, another
        tIm - year, second, minute; measure of time.
        past - previous, past, before
        naw  - now, still, present
        fUtr - future, following
          
        
        nul - no, none, zero
        wun - a, one, first
        sek - 2, second, latter
        tr  - 3, few
        kwa - 4, several
        bit - 8
        heks - 16
        glob - 32
        kyUb - 64
        den  - 128
        lot  - 256
        nekst - next 
        last - last, ending, final
        iny - any, some 
        xE - the, this, that 
        ol - all, every, ever, infinity, each 
        uxr - other, rest, alternatve, disjunction
        Abil - can, able, possible, probable
        not - not, no 
        sUdO - soon, about, approximately, roughly, almost
        
        sO - so, very 
        wiq - what, which, y/n question 
        mini - many, a lot of 
        resip - point, 1/
        Enuf - enough, sufficient
        sumwat - somewhat, moderately
        
        
        
        
        
        
        
        
        bI - by, among, through, using, according
        abaut - about, regarding, around, surrounding
        amung - among, between, in middle of 
        fa    - far, against, out  
        nrx - north, front
        sox - south, behind 
        Est - east, right
        west - west, left
        up   - high, over, top, up
        lO   - low, down, under, bottom 
        in   - in, at, on, with, here
        fr - for, from, per, so that
        from - of, from, originating or deriving from
        lIk - as, like, than, to similar degree/extent
        xat - such that, fulfilling, whatever
        if - if, because, so, unless, provided, since
        but - but, without, exception, disjunction 
        xO - though, despite, whereas, even
        and - and, intersection, minimum, simultaneously
        r   - or, union, maximum, nor 
        ekwil - same, equal
        dif  - different, opposite, instead 
        Exr  - either, xor
        until - until, while
        wix - with, using
        
        
        
        
        
        
        
        of - off, deactivated, sleeping          | on - on, awake, activated
        rlE - before, early                      | lAt - after, late 
        slO - slow                               | fast - fast, quick, frequent, sudden
        soft - delicate, soft, volatile          | stAbil - hard, stable
        xik  - thick, fat, wide                  | xin - skinny, narrow, thin, diet
        big - big, tall, large, long             | smal - small, short
        compOs - married, composite              | prIm - prime, single, divorced
        strOng - strong                          | wEk - weak
        sAf - safe                               | dAnj - dangerous
        carp - sharp                             | dul - dull, blunt
        frtIl - straight, fertile                | gA - gay, LGBT, barren
        qEp - cheap, poor                        | riq - fancy, rich, royal, elegant
        nU - new, fresh, young                   | old - old, mature
        klEn - clean                             | drtI - dirty, nasty, disgusting
        avrij - average, common, normal          | strAnj - special, strange, odd
        wIz   - wise, smart                      | dum - dumb
        Open  - open, public                     | nepO - closed, private 
        flat - flat                              | smUx - round, smooth
        ful  - full, complete, saturated         | holO - hollow, empty, vapid
        drekt - straight, vertical, direct       | bint - bent, diagonal, crooked
        hot - hot, warm                          | kuld - cold, raw
        wet - wet                                | drI - dry 
        frwrd - forward, ordered, organized      | bakwrd - backward, reversed
        spOs - supposed, evident, dubious, vague | obvI   - obvious, clear  
        fAk - figurative, metaphorical,          | litril - literal, actual, physical, tangible
              theoretical, ephemeral             |
        kemikal - chemical, electronic, natural  | natril - organic, natural, acoustic
        prilel  - parallel                       | krosin - intersecting 
        elastik - elastic, malleable,            | stif - rigid, tight, viscous
                  polymorphic, loose             |
        dEp - complex, difficult, deep           | simpil - simple, easy, shallow
        srtin - certain, sure, definite          | stukast - random, stochastic, unsure
        trU - true, correct                      | fals - false, wrong
        jenral - general, absolute               | prtik - particular, specific, relative 
        gud - good, healthy, important           | bad - bad, detrimental
        nesrE - important, necessary             | vestij - frivolous, vestigial, extra 
        nIs - kind, nice                         | krUl - cruel, mean
        srEus - serious, responsible             | silE - foolish, silly
        beluved - dear, precious                 | dredful - wretched, evil
        cOzE - comfortable, cosy                 | tuf     - rough, uncomfortable
        yond - sacred, beyond, holy              | fInit - discrete, finite
        bawnd - bound, trapped                   | lEs   - free, released
        kUl   - cool, awesome, based, popular    | lAm   - stuffy, square, unpopular
        glU   - sticky, magnetic                 | slip  - oily, slippery
        
        
        klEr - colorless, clear 
        kulr - colorful, colored 
        blak - dark, black 
        wIt  - light, white, bright 
        yelO - orange, brown, yellow
        red  - red 
        grEn - green
        blU  - blue, grey, dull 
        pEnk - purple, magenta, pink
        
        
        
        glad - happy, pleased, prideful
        sad  - sad, glum, depressed
        frAd - afraid, fearful, paranoid, anxious
        mad  - angry, enraged, frustrated, annoyed
        disgust - disgusted, hateful
        
        xank - grateful, thankful
        gilt - regretful, guilty, ashamed, embarrased
        srpIs - surprised, shocked
        krEus - curious, interested    
        alUf - bored, sarcastic, ennui-laden
        
        
        
        sAvrE - savory 
        swEt - sweet 
        bitr - bitter 
        sawr - sour 
        
        
    
        
        bE - be, live, be conscious, exist, become
        dU - do, enact, begin, start
        hav - have, control, own 
        mA - may, have permission to 
        wil - will, prepare, plan 
        kan - can, should, be able to
        cud - shall, should, ought to
        must - must, need, require, gotta
        
        
        mAk - make, birth, instantiate, invent, generate  
        stop - stop, abort, strike, end, limit, suspend
        qanj - modify, change, exchange, update
        send - send, give, post, transmit, distribute, publish, upload
        Enkr - increase, increment, grow, develop, add 
        dEkr - decrease, take, degrade, metabolize, subtract
        kopE - replicate, multiply, copy
        div - divide, split     
        git - get, inherit, download, win, keep
        sit - put, place
        lUz - lose, no longer have
        roll, turn, twist, shake, rotate
        distrb - repulse, disturb, bother
        wU - attract, woo
        sel - offer, sell
        qUz - select, choose, judge, determine, decide
        puc - push, coerce, force, lift
        test - test, stretch
        wont - desire, want, hope, envy
        fix - sort, organize, manage, maintain, fix
        oprAt - drive, sail, operate, use
        Uz    - ride, hang
        nEd   - need, require
    
        spEk - say, talk, discuss, transmit, message, call, ask
        kum - come, approach
        gO - go, travel  
        let - let, allow, permit, excuse, permit
        kinO - know, recognize
        wAt - wait, hold on
        kunsUm - eat, drink, consume, digest
        fOkus - focus, consider, feel an emotion
        xEnk - think, imagine, conjecture, believe
        hold - hold, grip, grab, hug, embrace, carry
        tuq - rub, bump, touch, feel, collide
        sEkrEt - cough, sneeze, blow, discharge, burst, secrete, erupt
        lisen - hear, listen, obey
        kis - kiss, fuck 
        sE - see, look, observe, notice 
        mUv - move, flow, drift, movement
        walk - step, walk, run
        swim - swim, fly  
        akt - act, behave
        slEp - sleep, rest, lay down
        wAk - wake, stand
        sEnk - sit, drop, sink, slip, fall
        jump - jump, hop
        sEm - seem, appear
        fInd - find, discover
        plA - play, dance, sing  
        trI - try, attempt
        luv - love, yearn
        lIk - enjoy, appreciate, like
        rEd - read, understand
        mEt - meet, greet, interview, court, date
        rIt - write, print, type
        lEd - teach, lead, inform, tell, influence
        lrn - learn, experiment, investigate
        dI - die, disappear, rot
    
    
        fIt - fight, compete, battle
        srq - hunt, search, pursue
        atak - attack, damage, kick, punch
        kil - kill, destroy, eliminate, crush, smash
        brn - cook, bake, broil, roast, fry, render, sear, barbecue, grill, burn
        pOq - boil,braise, simmer, poach
        sOk - soak, pickle, mascerate
        gambil - risk, gamble, dare
    
    
        na  - no
        yes - yes
        Ayo - hello
        bI - goodbye
        plEz - please
        xAnks - "thank you"
        yA - hooray, cheers, toast, woo! let's go!
        polujI - sorry
        gratsI - congratulations
        awrya - "how are you?"
        omI - oy vey! dios mio! Oh my!
        hu - what? huh?
        prden - excuse me
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        glisrEn - glycerine; artificial sweetener or vape juice
