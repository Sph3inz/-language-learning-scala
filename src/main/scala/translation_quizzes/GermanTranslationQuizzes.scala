package languagelearningbot.translation_quizzes

import languagelearningbot._

/**
 * German Translation Quizzes
 * Contains translation quiz questions for German in all difficulty levels
 */
object GermanTranslationQuizzes {
  
  // Get German translation quizzes for a specific difficulty level
  def getQuizzes(difficulty: Difficulty): List[List[Question]] = {
    difficulty match {
      case Easy => easyQuizzes
      case Medium => mediumQuizzes
      case Hard => hardQuizzes
      case Impossible => impossibleQuizzes
    }
  }
  
  // German translation quizzes - Easy difficulty (3 versions)
  private val easyQuizzes: List[List[Question]] = List(
    // Version 1 - Basic greetings and phrases
    List(
      Question("How do you say 'Hello, how are you?' in German?", List("Hallo, wie geht es dir?", "Guten Tag, wie geht's?", "Hallo, wie geht's?", "All of these are correct"), "All of these are correct", Translation),
      Question("Translate 'My name is John' to German", List("Ich heiße John", "Mein Name ist John", "Ich bin John", "All of these are acceptable"), "Ich heiße John", Translation),
      Question("Translate 'I would like a coffee, please' to German", List("Ich hätte gerne einen Kaffee, bitte", "Ich möchte einen Kaffee, bitte", "Ich würde gerne einen Kaffee haben, bitte", "All of these are correct"), "Ich hätte gerne einen Kaffee, bitte", Translation),
      Question("How do you say 'Where is the bathroom?' in German?", List("Wo ist die Toilette?", "Wo ist das Badezimmer?", "Wo ist das WC?", "All of these are acceptable"), "Wo ist die Toilette?", Translation),
      Question("Translate 'Thank you very much' to German", List("Vielen Dank", "Danke schön", "Danke sehr", "All of these are correct"), "Vielen Dank", Translation)
    ),
    
    // Version 2 - Food and restaurants
    List(
      Question("Translate 'I would like to order' to German", List("Ich möchte bestellen", "Ich würde gerne bestellen", "Ich hätte gerne bestellt", "All of these are correct"), "Ich möchte bestellen", Translation),
      Question("How do you say 'The bill, please' in German?", List("Die Rechnung, bitte", "Die Quittung, bitte", "Zahlen, bitte", "All of these are acceptable"), "Die Rechnung, bitte", Translation),
      Question("Translate 'Is there a vegetarian option?' to German", List("Gibt es vegetarische Optionen?", "Haben Sie vegetarische Gerichte?", "Gibt es etwas für Vegetarier?", "All of these are correct"), "Gibt es vegetarische Optionen?", Translation),
      Question("How do you say 'This food is delicious' in German?", List("Dieses Essen ist köstlich", "Das schmeckt sehr gut", "Das Essen ist lecker", "All of these are correct"), "Dieses Essen ist köstlich", Translation),
      Question("Translate 'I am allergic to nuts' to German", List("Ich bin allergisch gegen Nüsse", "Ich habe eine Nussallergie", "Ich vertrage keine Nüsse", "All of these convey the same meaning"), "Ich bin allergisch gegen Nüsse", Translation)
    ),
    
    // Version 3 - Travel and directions
    List(
      Question("How do you say 'How much does this cost?' in German?", List("Wie viel kostet das?", "Was kostet das?", "Wie teuer ist das?", "All of these are correct"), "All of these are correct", Translation),
      Question("Translate 'I need a hotel' to German", List("Ich brauche ein Hotel", "Ich benötige ein Hotel", "Ich suche ein Hotel", "All of these are correct"), "Ich brauche ein Hotel", Translation),
      Question("How do you say 'Turn right at the corner' in German?", List("Biegen Sie rechts an der Ecke ab", "Rechts abbiegen an der Ecke", "An der Ecke nach rechts", "All of these convey the instruction"), "Biegen Sie rechts an der Ecke ab", Translation),
      Question("Translate 'Is it far from here?' to German", List("Ist es weit von hier?", "Ist es weit entfernt?", "Ist es weit weg?", "All of these are correct"), "Ist es weit von hier?", Translation),
      Question("How do you say 'I am lost' in German?", List("Ich habe mich verirrt", "Ich bin verloren", "Ich weiß nicht, wo ich bin", "All of these convey the same meaning"), "Ich habe mich verirrt", Translation)
    )
  )
  
  // German translation quizzes - Medium difficulty (3 versions)
  private val mediumQuizzes: List[List[Question]] = List(
    // Version 1 - Everyday situations
    List(
      Question("Translate 'I've been waiting for 20 minutes' to German", List("Ich warte seit 20 Minuten", "Ich habe 20 Minuten gewartet", "Ich warte schon 20 Minuten lang", "All of these convey waiting time"), "Ich warte seit 20 Minuten", Translation),
      Question("How would you say 'Could you speak more slowly, please?' in German?", List("Könnten Sie bitte langsamer sprechen?", "Können Sie bitte langsamer sprechen?", "Sprechen Sie bitte langsamer", "All of these are acceptable"), "Könnten Sie bitte langsamer sprechen?", Translation),
      Question("Translate 'I don't understand what you're saying' to German", List("Ich verstehe nicht, was Sie sagen", "Ich verstehe dich nicht", "Ich kann nicht verstehen, was du meinst", "All of these are correct depending on formality"), "Ich verstehe nicht, was Sie sagen", Translation),
      Question("How would you translate 'We should meet tomorrow' to German?", List("Wir sollten uns morgen treffen", "Wir müssen uns morgen sehen", "Lasst uns morgen treffen", "All of these convey the suggestion"), "Wir sollten uns morgen treffen", Translation),
      Question("Translate 'What time does the store open?' to German", List("Wann öffnet das Geschäft?", "Um wie viel Uhr öffnet der Laden?", "Zu welcher Zeit macht das Geschäft auf?", "All of these are correct"), "Wann öffnet das Geschäft?", Translation)
    ),
    
    // Version 2 - Health and emergencies
    List(
      Question("How would you say 'I need to see a doctor' in German?", List("Ich muss einen Arzt sehen", "Ich brauche einen Arzt", "Ich benötige ärztliche Hilfe", "All of these convey the need"), "Ich muss einen Arzt sehen", Translation),
      Question("Translate 'I have a headache' to German", List("Ich habe Kopfschmerzen", "Mir tut der Kopf weh", "Ich habe Schmerzen im Kopf", "All of these convey the pain"), "Ich habe Kopfschmerzen", Translation),
      Question("How would you say 'Where is the nearest pharmacy?' in German?", List("Wo ist die nächste Apotheke?", "Wo finde ich die nächste Apotheke?", "Wo gibt es hier eine Apotheke?", "All of these ask for a pharmacy"), "Wo ist die nächste Apotheke?", Translation),
      Question("Translate 'I've lost my passport' to German", List("Ich habe meinen Pass verloren", "Mein Reisepass ist weg", "Ich finde meinen Pass nicht mehr", "All of these convey the issue"), "Ich habe meinen Pass verloren", Translation),
      Question("How would you say 'Call an ambulance!' in German?", List("Rufen Sie einen Krankenwagen!", "Rufen Sie einen Notarzt!", "Holen Sie schnell einen Rettungswagen!", "All of these are acceptable in an emergency"), "Rufen Sie einen Krankenwagen!", Translation)
    ),
    
    // Version 3 - Work and business
    List(
      Question("Translate 'I would like to schedule a meeting' to German", List("Ich möchte ein Treffen vereinbaren", "Ich würde gerne ein Meeting planen", "Ich hätte gern einen Termin ausgemacht", "All of these convey scheduling"), "Ich möchte ein Treffen vereinbaren", Translation),
      Question("How would you say 'Please send me the documents by email' in German?", List("Bitte senden Sie mir die Unterlagen per E-Mail", "Bitte schicken Sie mir die Dokumente per E-Mail", "Würden Sie mir bitte die Unterlagen per E-Mail zusenden?", "All of these are acceptable"), "Bitte senden Sie mir die Unterlagen per E-Mail", Translation),
      Question("Translate 'We need to discuss this project' to German", List("Wir müssen dieses Projekt besprechen", "Wir sollten über dieses Projekt diskutieren", "Wir müssen über dieses Projekt reden", "All of these convey the need"), "Wir müssen dieses Projekt besprechen", Translation),
      Question("How would you say 'I'm interested in your proposal' in German?", List("Ich bin an Ihrem Vorschlag interessiert", "Mich interessiert Ihr Angebot", "Ihr Vorschlag klingt interessant für mich", "All of these convey interest"), "Ich bin an Ihrem Vorschlag interessiert", Translation),
      Question("Translate 'Could you explain that again?' to German", List("Könnten Sie das noch einmal erklären?", "Würden Sie das bitte wiederholen?", "Können Sie das nochmal erläutern?", "All of these ask for repetition"), "Könnten Sie das noch einmal erklären?", Translation)
    )
  )
  
  // German translation quizzes - Hard difficulty (3 versions)
  private val hardQuizzes: List[List[Question]] = List(
    // Version 1 - Complex social interactions
    List(
      Question("Translate 'I'd appreciate it if you could help me with this matter' to German", List("Ich würde es schätzen, wenn Sie mir in dieser Angelegenheit helfen könnten", "Ich wäre Ihnen dankbar, wenn Sie mir bei dieser Sache behilflich sein könnten", "Es wäre mir eine große Hilfe, wenn Sie sich dieser Angelegenheit annehmen könnten", "All of these are formal and correct"), "Ich würde es schätzen, wenn Sie mir in dieser Angelegenheit helfen könnten", Translation),
      Question("How would you translate 'We regret to inform you that your application has been rejected' to German?", List("Wir bedauern, Ihnen mitteilen zu müssen, dass Ihre Bewerbung abgelehnt wurde", "Leider müssen wir Ihnen mitteilen, dass wir Ihre Bewerbung nicht berücksichtigen können", "Mit Bedauern informieren wir Sie, dass Ihre Bewerbung nicht erfolgreich war", "All of these convey the rejection formally"), "Wir bedauern, Ihnen mitteilen zu müssen, dass Ihre Bewerbung abgelehnt wurde", Translation),
      Question("Translate 'It's not what you said, but how you said it' to German", List("Es ist nicht, was du gesagt hast, sondern wie du es gesagt hast", "Nicht der Inhalt, sondern der Ton macht die Musik", "Es geht nicht um die Worte, sondern um die Art und Weise", "All of these convey the sentiment"), "Es ist nicht, was du gesagt hast, sondern wie du es gesagt hast", Translation),
      Question("How would you say 'Let's agree to disagree' in German?", List("Lassen wir uns darauf einigen, unterschiedlicher Meinung zu sein", "Wir können uns darauf einigen, dass wir uns nicht einig sind", "Akzeptieren wir, dass wir verschiedene Ansichten haben", "All of these express the idea"), "Lassen wir uns darauf einigen, unterschiedlicher Meinung zu sein", Translation),
      Question("Translate 'I didn't mean to offend you' to German", List("Ich wollte dich nicht beleidigen", "Es war nicht meine Absicht, dich zu kränken", "Ich habe nicht beabsichtigt, dich zu verletzen", "All of these convey the apology"), "Ich wollte dich nicht beleidigen", Translation)
    ),
    
    // Version 2 - Cultural and idiomatic expressions
    List(
      Question("How would you translate the idiomatic expression 'It's raining cats and dogs' to German?", List("Es gießt wie aus Eimern", "Es regnet Bindfäden", "Es schüttet wie aus Kübeln", "All of these convey heavy rain"), "Es gießt wie aus Eimern", Translation),
      Question("Translate 'You can't have your cake and eat it too' to German", List("Man kann nicht alles haben", "Wasch mir den Pelz, aber mach mich nicht nass", "Du kannst nicht auf zwei Hochzeiten tanzen", "All of these convey the meaning"), "Man kann nicht alles haben", Translation),
      Question("How would you say 'That's the last straw' in German?", List("Das ist der Tropfen, der das Fass zum Überlaufen bringt", "Das schlägt dem Fass den Boden aus", "Jetzt reicht's aber wirklich", "All of these express the idiom"), "Das ist der Tropfen, der das Fass zum Überlaufen bringt", Translation),
      Question("Translate 'To beat around the bush' to German", List("Um den heißen Brei herumreden", "Nicht zum Punkt kommen", "Wie die Katze um den heißen Brei", "All of these convey the meaning"), "Um den heißen Brei herumreden", Translation),
      Question("How would you translate 'When pigs fly' to German?", List("Wenn Schweine fliegen können", "Am Sankt-Nimmerleins-Tag", "Wenn Ostern und Pfingsten auf einen Tag fallen", "All of these express impossibility"), "Wenn Schweine fliegen können", Translation)
    ),
    
    // Version 3 - Abstract concepts
    List(
      Question("Translate 'The meaning of life is a philosophical question that has been debated throughout history' to German", List("Die Bedeutung des Lebens ist eine philosophische Frage, die während der gesamten Geschichte diskutiert wurde", "Der Sinn des Lebens ist eine philosophische Fragestellung, die im Laufe der Geschichte debattiert wurde", "Die Frage nach dem Lebenssinn ist ein philosophisches Thema, das historisch immer wieder erörtert wurde", "All of these convey the concept"), "Die Bedeutung des Lebens ist eine philosophische Frage, die während der gesamten Geschichte diskutiert wurde", Translation),
      Question("How would you translate 'Consciousness remains one of the biggest mysteries in science' to German?", List("Das Bewusstsein bleibt eines der größten Rätsel der Wissenschaft", "Das Bewusstsein ist nach wie vor eines der größten wissenschaftlichen Mysterien", "Das Bewusstsein gehört weiterhin zu den größten Geheimnissen der Wissenschaft", "All of these express the concept"), "Das Bewusstsein bleibt eines der größten Rätsel der Wissenschaft", Translation),
      Question("Translate 'Sustainable development aims to meet human needs while preserving the environment' to German", List("Nachhaltige Entwicklung zielt darauf ab, menschliche Bedürfnisse zu erfüllen und gleichzeitig die Umwelt zu schützen", "Nachhaltige Entwicklung versucht, die Bedürfnisse der Menschen zu befriedigen, während sie die Umwelt bewahrt", "Nachhaltige Entwicklung strebt an, menschliche Bedürfnisse zu decken und dabei die Umwelt zu erhalten", "All of these convey the concept"), "Nachhaltige Entwicklung zielt darauf ab, menschliche Bedürfnisse zu erfüllen und gleichzeitig die Umwelt zu schützen", Translation),
      Question("How would you say 'Democracy is based on the principle of rule by the people' in German?", List("Demokratie basiert auf dem Prinzip der Herrschaft durch das Volk", "Demokratie gründet sich auf dem Grundsatz der Volksherrschaft", "Demokratie beruht auf dem Prinzip, dass das Volk regiert", "All of these express the concept"), "Demokratie basiert auf dem Prinzip der Herrschaft durch das Volk", Translation),
      Question("Translate 'The concept of justice varies across different cultures and historical periods' to German", List("Das Konzept der Gerechtigkeit variiert zwischen verschiedenen Kulturen und historischen Epochen", "Die Vorstellung von Gerechtigkeit unterscheidet sich je nach Kultur und geschichtlicher Periode", "Der Begriff der Gerechtigkeit ist in verschiedenen Kulturen und geschichtlichen Zeiträumen unterschiedlich", "All of these convey the concept"), "Das Konzept der Gerechtigkeit variiert zwischen verschiedenen Kulturen und historischen Epochen", Translation)
    )
  )
  
  // German translation quizzes - Impossible difficulty (3 versions)
  private val impossibleQuizzes: List[List[Question]] = List(
    // Version 1 - Technical and specialized language
    List(
      Question("Translate the legal term 'The burden of proof rests with the prosecution' to German", List("Die Beweislast liegt bei der Staatsanwaltschaft", "Die Beweispflicht obliegt der Anklage", "Die Beweislast trägt die Strafverfolgungsbehörde", "All of these convey the legal concept"), "Die Beweislast liegt bei der Staatsanwaltschaft", Translation),
      Question("How would you translate the medical term 'The patient presents with acute myocardial infarction requiring immediate intervention' to German?", List("Der Patient weist einen akuten Myokardinfarkt auf, der einen sofortigen Eingriff erfordert", "Der Patient leidet an einem akuten Herzinfarkt, der eine umgehende Intervention notwendig macht", "Der Patient zeigt Anzeichen eines akuten Myokardinfarkts, der eine sofortige Behandlung erfordert", "All of these express the medical terminology"), "Der Patient weist einen akuten Myokardinfarkt auf, der einen sofortigen Eingriff erfordert", Translation),
      Question("Translate the technical computing term 'The algorithm optimizes memory allocation through dynamic garbage collection' to German", List("Der Algorithmus optimiert die Speicherzuweisung durch dynamische Speicherbereinigung", "Der Algorithmus verbessert die Speicherzuteilung mittels dynamischer Garbage Collection", "Der Algorithmus optimiert die Speicherallokation durch dynamische Freispeichersammlung", "All of these express the technical concept"), "Der Algorithmus optimiert die Speicherzuweisung durch dynamische Speicherbereinigung", Translation),
      Question("How would you translate the financial term 'The leveraged buyout resulted in significant corporate restructuring' to German?", List("Die fremdfinanzierte Übernahme führte zu einer erheblichen Unternehmensumstrukturierung", "Der Leveraged Buyout resultierte in einer bedeutenden Neustrukturierung des Unternehmens", "Die kreditfinanzierte Akquisition hatte eine wesentliche Restrukturierung des Konzerns zur Folge", "All of these convey the financial concept"), "Die fremdfinanzierte Übernahme führte zu einer erheblichen Unternehmensumstrukturierung", Translation),
      Question("Translate the philosophical concept 'Existentialism posits that individuals create meaning in an otherwise meaningless universe' to German", List("Der Existentialismus postuliert, dass Individuen in einem ansonsten sinnlosen Universum Bedeutung schaffen", "Der Existentialismus behauptet, dass Menschen in einem grundsätzlich sinnlosen Universum Sinn erzeugen", "Der Existentialismus geht davon aus, dass Einzelpersonen in einem sonst bedeutungslosen Kosmos Sinn stiften", "All of these express the philosophical concept"), "Der Existentialismus postuliert, dass Individuen in einem ansonsten sinnlosen Universum Bedeutung schaffen", Translation)
    ),
    
    // Version 2 - Literary and poetic language
    List(
      Question("Translate this line by Goethe to English: 'Über allen Gipfeln ist Ruh'", List("Over all the peaks is peace", "Above all summits is stillness", "O'er all the hilltops is quiet now", "All of these capture the poetic essence"), "Over all the peaks is peace", Translation),
      Question("How would you translate Shakespeare's 'To be or not to be, that is the question' to German preserving its poetic quality?", List("Sein oder Nichtsein, das ist hier die Frage", "Sein oder nicht sein, das ist die Frage", "Zu sein oder nicht zu sein, das ist die Frage", "All of these preserve the poetic quality"), "Sein oder Nichtsein, das ist hier die Frage", Translation),
      Question("Translate the metaphorical expression 'Time is a thief that leaves no fingerprints' to German", List("Die Zeit ist ein Dieb, der keine Fingerabdrücke hinterlässt", "Zeit ist ein Dieb ohne Spuren", "Die Zeit stiehlt, ohne Beweise zu hinterlassen", "All of these preserve the metaphor"), "Die Zeit ist ein Dieb, der keine Fingerabdrücke hinterlässt", Translation),
      Question("How would you translate the poetic phrase 'The whispers of autumn leaves dancing in the wind' to German?", List("Das Flüstern der Herbstblätter, die im Wind tanzen", "Das Raunen der herbstlichen Blätter, die mit dem Wind tanzen", "Das leise Rascheln tanzender Herbstblätter im Wind", "All of these preserve the poetic quality"), "Das Flüstern der Herbstblätter, die im Wind tanzen", Translation),
      Question("Translate the literary quote 'In the midst of winter, I found there was, within me, an invincible summer' to German", List("Mitten im Winter entdeckte ich, dass in mir ein unbesiegbarer Sommer wohnt", "Inmitten des Winters fand ich heraus, dass in mir ein unbesiegbarer Sommer war", "Im tiefsten Winter wurde mir bewusst, dass in mir ein unbesiegbarer Sommer liegt", "All of these preserve the literary quality"), "Mitten im Winter entdeckte ich, dass in mir ein unbesiegbarer Sommer wohnt", Translation)
    ),
    
    // Version 3 - Regional variations and dialects
    List(
      Question("Translate the Austrian German phrase 'Grüß Gott!' to standard German", List("Guten Tag!", "Hallo!", "Grüße dich!", "All of these are standard greetings"), "Guten Tag!", Translation),
      Question("How would you say 'I'd like a bread roll' in Swiss German dialect as opposed to standard German?", List("I möcht es Brötli", "Ich hätte gerne ein Brötchen", "Ich möchte eine Semmel", "Ich will ein Weckerl"), "I möcht es Brötli", Translation),
      Question("Translate the Bavarian phrase 'Servus, wie geht's dir?' to standard German", List("Hallo, wie geht es dir?", "Guten Tag, wie geht's?", "Grüß dich, wie geht's dir?", "All of these convey the same greeting"), "Hallo, wie geht es dir?", Translation),
      Question("How would you translate the Berlin slang 'Dit is' ja dufte!' to standard German?", List("Das ist ja toll!", "Das ist ja super!", "Das ist ja großartig!", "All of these convey the enthusiasm"), "Das ist ja toll!", Translation),
      Question("Translate the Northern German phrase 'Moin moin!' to standard German", List("Guten Tag!", "Guten Morgen!", "Hallo!", "All of these are acceptable"), "Guten Tag!", Translation)
    )
  )
}
