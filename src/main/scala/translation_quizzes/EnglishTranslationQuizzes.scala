package languagelearningbot.translation_quizzes

import languagelearningbot._

/**
 * English Translation Quizzes
 * Contains translation quiz questions for English in all difficulty levels
 */
object EnglishTranslationQuizzes {
  
  // Get English translation quizzes for a specific difficulty level
  def getQuizzes(difficulty: Difficulty): List[List[Question]] = {
    difficulty match {
      case Easy => easyQuizzes
      case Medium => mediumQuizzes
      case Hard => hardQuizzes
      case Impossible => impossibleQuizzes
    }
  }
  
  // English translation quizzes - Easy difficulty (3 versions)
  private val easyQuizzes: List[List[Question]] = List(
    // Version 1 - Basic greetings and phrases
    List(
      Question("How do you say 'Hello, how are you?' in English?", List("Hello, how are you?", "Hi, how are you doing?", "Hey, what's up?", "All of these are correct"), "All of these are correct", Translation),
      Question("Translate 'Me llamo John' from Spanish to English", List("My name is John", "I call myself John", "I am named John", "They call me John"), "My name is John", Translation),
      Question("Translate 'Je voudrais un café, s'il vous plaît' from French to English", List("I would like a coffee, please", "I want a coffee, please", "I'd like to have a coffee, please", "All of these are correct"), "I would like a coffee, please", Translation),
      Question("How do you say 'Wo ist die Toilette?' from German in English?", List("Where is the toilet?", "Where is the bathroom?", "Where is the restroom?", "All of these are correct"), "All of these are correct", Translation),
      Question("Translate 'شكرا جزيلا' from Arabic to English", List("Thank you very much", "Thanks a lot", "Many thanks", "All of these are correct"), "Thank you very much", Translation)
    ),
    
    // Version 2 - Food and restaurants
    List(
      Question("Translate 'Quisiera ordenar' from Spanish to English", List("I would like to order", "I want to order", "I'd like to place an order", "All of these are correct"), "I would like to order", Translation),
      Question("How do you say 'L'addition, s'il vous plaît' from French in English?", List("The bill, please", "The check, please", "The tab, please", "All of these are correct"), "All of these are correct", Translation),
      Question("Translate 'Gibt es vegetarische Optionen?' from German to English", List("Are there vegetarian options?", "Do you have vegetarian options?", "Is there anything vegetarian?", "All of these are correct"), "Are there vegetarian options?", Translation),
      Question("How do you say 'هذا الطعام لذيذ' from Arabic in English?", List("This food is delicious", "This meal is tasty", "This dish is very good", "All of these convey the same meaning"), "This food is delicious", Translation),
      Question("Translate 'Sono allergico alle noci' from Italian to English", List("I am allergic to nuts", "I have a nut allergy", "Nuts give me an allergic reaction", "All of these convey the same meaning"), "I am allergic to nuts", Translation)
    ),
    
    // Version 3 - Travel and directions
    List(
      Question("How do you say '¿Cuánto cuesta esto?' from Spanish in English?", List("How much does this cost?", "How much is this?", "What's the price of this?", "All of these are correct"), "All of these are correct", Translation),
      Question("Translate 'J'ai besoin d'un hôtel' from French to English", List("I need a hotel", "I require a hotel", "I'm looking for a hotel", "All of these are correct"), "I need a hotel", Translation),
      Question("How do you say 'Biegen Sie rechts an der Ecke ab' from German in English?", List("Turn right at the corner", "Make a right at the corner", "Take a right at the corner", "All of these are correct"), "All of these are correct", Translation),
      Question("Translate 'هل هذا بعيد من هنا؟' from Arabic to English", List("Is this far from here?", "Is it far from here?", "How far is it from here?", "All of these convey the same meaning"), "Is this far from here?", Translation),
      Question("How do you say 'Mi sono perso' from Italian in English?", List("I am lost", "I've gotten lost", "I've lost my way", "All of these are correct"), "All of these are correct", Translation)
    )
  )
  
  // English translation quizzes - Medium difficulty (3 versions)
  private val mediumQuizzes: List[List[Question]] = List(
    // Version 1 - Everyday situations
    List(
      Question("Translate 'Llevo 20 minutos esperando' from Spanish to English", List("I've been waiting for 20 minutes", "I have waited 20 minutes", "I'm waiting for 20 minutes now", "I've spent 20 minutes waiting"), "I've been waiting for 20 minutes", Translation),
      Question("How would you translate 'Könnten Sie bitte langsamer sprechen?' from German to English?", List("Could you please speak more slowly?", "Would you mind speaking more slowly?", "Can you speak slower, please?", "All of these are correct"), "Could you please speak more slowly?", Translation),
      Question("Translate 'Je ne comprends pas ce que vous dites' from French to English", List("I don't understand what you're saying", "I don't understand what you say", "I can't understand your words", "I don't comprehend what you're telling me"), "I don't understand what you're saying", Translation),
      Question("How would you translate 'يجب أن نلتقي غدا' from Arabic to English?", List("We should meet tomorrow", "We must meet tomorrow", "We have to meet tomorrow", "All of these convey the same meaning"), "We should meet tomorrow", Translation),
      Question("Translate 'A che ora apre il negozio?' from Italian to English", List("What time does the store open?", "When does the shop open?", "At what hour does the store open?", "All of these are correct"), "What time does the store open?", Translation)
    ),
    
    // Version 2 - Health and emergencies
    List(
      Question("How would you translate 'Necesito ver a un médico' from Spanish to English?", List("I need to see a doctor", "I need a physician", "I must consult a doctor", "All of these are correct"), "I need to see a doctor", Translation),
      Question("Translate 'J'ai mal à la tête' from French to English", List("I have a headache", "My head hurts", "I have a pain in my head", "All of these convey the same meaning"), "I have a headache", Translation),
      Question("How would you say 'Wo ist die nächste Apotheke?' from German in English?", List("Where is the nearest pharmacy?", "Where is the closest drugstore?", "Where can I find the nearest chemist?", "All of these are correct"), "Where is the nearest pharmacy?", Translation),
      Question("Translate 'لقد فقدت جواز سفري' from Arabic to English", List("I've lost my passport", "I lost my passport", "My passport is lost", "All of these convey the same meaning"), "I've lost my passport", Translation),
      Question("How would you translate 'Chiamate un'ambulanza!' from Italian to English?", List("Call an ambulance!", "Get an ambulance!", "Phone for an ambulance!", "All of these are acceptable in an emergency"), "Call an ambulance!", Translation)
    ),
    
    // Version 3 - Work and business
    List(
      Question("Translate 'Me gustaría programar una reunión' from Spanish to English", List("I would like to schedule a meeting", "I'd like to arrange a meeting", "I want to set up a meeting", "All of these are correct"), "I would like to schedule a meeting", Translation),
      Question("How would you say 'Bitte senden Sie mir die Unterlagen per E-Mail' from German in English?", List("Please send me the documents by email", "Please email me the documents", "Please forward the documents to me via email", "All of these are correct"), "Please send me the documents by email", Translation),
      Question("Translate 'Nous devons discuter de ce projet' from French to English", List("We need to discuss this project", "We must talk about this project", "We have to discuss this project", "All of these are correct"), "We need to discuss this project", Translation),
      Question("How would you translate 'أنا مهتم باقتراحك' from Arabic to English?", List("I'm interested in your proposal", "I am interested in your suggestion", "Your proposal interests me", "All of these convey the same meaning"), "I'm interested in your proposal", Translation),
      Question("Translate 'Potresti spiegare di nuovo?' from Italian to English", List("Could you explain again?", "Could you explain that again?", "Would you mind explaining that again?", "All of these are correct"), "Could you explain that again?", Translation)
    )
  )
  
  // English translation quizzes - Hard difficulty (3 versions)
  private val hardQuizzes: List[List[Question]] = List(
    // Version 1 - Complex social interactions
    List(
      Question("Translate 'Agradecería que me pudiera ayudar con este asunto' from Spanish to English", List("I'd appreciate it if you could help me with this matter", "I would be grateful if you could assist me with this issue", "I would appreciate your help with this matter", "All of these convey the formal request"), "I'd appreciate it if you could help me with this matter", Translation),
      Question("How would you translate 'Wir bedauern, Ihnen mitteilen zu müssen, dass Ihre Bewerbung abgelehnt wurde' from German to English?", List("We regret to inform you that your application has been rejected", "We are sorry to tell you that your application has been declined", "Unfortunately, we must inform you that your application was not successful", "All of these convey the formal message"), "We regret to inform you that your application has been rejected", Translation),
      Question("Translate 'Ce n'est pas ce que vous avez dit, mais comment vous l'avez dit' from French to English", List("It's not what you said, but how you said it", "It's not the content but the tone of your message", "It's not your words but the way you expressed them", "All of these convey the same meaning"), "It's not what you said, but how you said it", Translation),
      Question("How would you say 'دعنا نتفق على أن نختلف' from Arabic in English?", List("Let's agree to disagree", "Let's accept our differences of opinion", "We can agree that we have different views", "All of these express the same idea"), "Let's agree to disagree", Translation),
      Question("Translate 'Non era mia intenzione offenderti' from Italian to English", List("I didn't mean to offend you", "It wasn't my intention to offend you", "I didn't intend to hurt your feelings", "All of these are correct"), "I didn't mean to offend you", Translation)
    ),
    
    // Version 2 - Cultural and idiomatic expressions
    List(
      Question("How would you translate the Spanish idiom 'Está lloviendo a cántaros' to English?", List("It's raining cats and dogs", "It's pouring", "It's raining heavily", "All of these convey the same meaning"), "It's raining cats and dogs", Translation),
      Question("Translate the French expression 'On ne peut pas avoir le beurre et l'argent du beurre' to English", List("You can't have your cake and eat it too", "You can't have it both ways", "You can't have the best of both worlds", "All of these express the same idea"), "You can't have your cake and eat it too", Translation),
      Question("How would you translate the German saying 'Das ist der Tropfen, der das Fass zum Überlaufen bringt' to English?", List("That's the last straw", "That's the drop that makes the barrel overflow", "That's what breaks the camel's back", "All of these express the same idea"), "That's the last straw", Translation),
      Question("Translate the Arabic expression 'يدور حول الموضوع' to English", List("To beat around the bush", "To not get to the point", "To skirt around the issue", "All of these convey the same meaning"), "To beat around the bush", Translation),
      Question("How would you translate the Italian saying 'Quando le galline avranno i denti' to English?", List("When pigs fly", "When hell freezes over", "When chickens have teeth", "All of these express impossibility"), "When pigs fly", Translation)
    ),
    
    // Version 3 - Abstract concepts
    List(
      Question("Translate 'El significado de la vida es una cuestión filosófica que ha sido debatida a lo largo de la historia' from Spanish to English", List("The meaning of life is a philosophical question that has been debated throughout history", "The purpose of life is a philosophical issue that has been debated through history", "The significance of life is a philosophical matter debated throughout history", "All of these convey the abstract concept"), "The meaning of life is a philosophical question that has been debated throughout history", Translation),
      Question("How would you translate 'Das Bewusstsein bleibt eines der größten Rätsel der Wissenschaft' from German to English?", List("Consciousness remains one of the greatest mysteries of science", "Consciousness continues to be one of science's biggest enigmas", "Awareness remains one of the greatest scientific mysteries", "All of these express the concept"), "Consciousness remains one of the greatest mysteries of science", Translation),
      Question("Translate 'Le développement durable vise à répondre aux besoins humains tout en préservant l'environnement' from French to English", List("Sustainable development aims to meet human needs while preserving the environment", "Sustainable development seeks to satisfy human needs while conserving the environment", "Sustainable development goals are to fulfill human needs while protecting the environment", "All of these convey the concept"), "Sustainable development aims to meet human needs while preserving the environment", Translation),
      Question("How would you say 'الديمقراطية تستند على مبدأ حكم الشعب' from Arabic in English?", List("Democracy is based on the principle of rule by the people", "Democracy is founded on the principle of governance by the people", "Democracy rests on the principle of the people's authority", "All of these express the concept"), "Democracy is based on the principle of rule by the people", Translation),
      Question("Translate 'Il concetto di giustizia varia tra culture diverse e periodi storici' from Italian to English", List("The concept of justice varies across different cultures and historical periods", "The notion of justice differs between cultures and historical eras", "The idea of justice changes across cultures and throughout history", "All of these convey the abstract concept"), "The concept of justice varies across different cultures and historical periods", Translation)
    )
  )
  
  // English translation quizzes - Impossible difficulty (3 versions)
  private val impossibleQuizzes: List[List[Question]] = List(
    // Version 1 - Technical and specialized language
    List(
      Question("Translate the Spanish legal term 'La carga de la prueba recae sobre la fiscalía' to English", List("The burden of proof rests with the prosecution", "The onus of proof falls on the prosecution", "The prosecution bears the burden of proof", "All of these convey the legal concept"), "The burden of proof rests with the prosecution", Translation),
      Question("How would you translate the German medical term 'Der Patient leidet an einem akuten Myokardinfarkt, der einen sofortigen Eingriff erfordert' to English?", List("The patient presents with acute myocardial infarction requiring immediate intervention", "The patient suffers from an acute myocardial infarction that necessitates immediate intervention", "The patient has an acute heart attack requiring immediate treatment", "All of these express the medical concept"), "The patient presents with acute myocardial infarction requiring immediate intervention", Translation),
      Question("Translate the French technical computing term 'L'algorithme optimise l'allocation de mémoire par le biais d'une collecte dynamique des déchets' to English", List("The algorithm optimizes memory allocation through dynamic garbage collection", "The algorithm improves memory allocation via dynamic garbage collection", "The algorithm enhances memory distribution through dynamic waste collection", "All of these express the technical concept"), "The algorithm optimizes memory allocation through dynamic garbage collection", Translation),
      Question("How would you translate the Arabic financial term 'أدت عملية الشراء بالرافعة المالية إلى إعادة هيكلة كبيرة للشركة' to English?", List("The leveraged buyout resulted in significant corporate restructuring", "The LBO led to substantial company reorganization", "The leveraged acquisition caused major corporate restructuring", "All of these convey the financial concept"), "The leveraged buyout resulted in significant corporate restructuring", Translation),
      Question("Translate the Italian philosophical concept 'L'esistenzialismo afferma che gli individui creano significato in un universo altrimenti privo di senso' to English", List("Existentialism posits that individuals create meaning in an otherwise meaningless universe", "Existentialism asserts that people generate meaning in an inherently meaningless universe", "Existentialism maintains that humans create significance in a fundamentally senseless universe", "All of these express the philosophical concept"), "Existentialism posits that individuals create meaning in an otherwise meaningless universe", Translation)
    ),
    
    // Version 2 - Literary and poetic language
    List(
      Question("Translate this line by Pablo Neruda from Spanish to English: 'Puedo escribir los versos más tristes esta noche'", List("Tonight I can write the saddest lines", "I can write the saddest verses tonight", "This night I can write the saddest poems", "All of these capture the poetic essence"), "Tonight I can write the saddest lines", Translation),
      Question("How would you translate Goethe's German line 'Über allen Gipfeln ist Ruh' to English preserving its poetic quality?", List("Over all the peaks is peace", "Above all summits is stillness", "O'er all the hilltops is quiet now", "All of these preserve the poetic quality"), "O'er all the hilltops is quiet now", Translation),
      Question("Translate the French metaphorical expression 'Le temps est un voleur qui ne laisse pas d'empreintes' to English", List("Time is a thief that leaves no fingerprints", "Time steals without leaving traces", "Time robs without leaving evidence", "All of these preserve the metaphor"), "Time is a thief that leaves no fingerprints", Translation),
      Question("How would you translate the Arabic poetic phrase 'همس أوراق الخريف الراقصة في الريح' to English?", List("The whispers of autumn leaves dancing in the wind", "The murmurs of fall leaves swaying in the breeze", "The rustle of autumn foliage dancing with the wind", "All of these preserve the poetic quality"), "The whispers of autumn leaves dancing in the wind", Translation),
      Question("Translate the Italian literary quote 'Nel mezzo dell'inverno, ho finalmente imparato che vi era in me un'estate invincibile' to English", List("In the midst of winter, I found there was, within me, an invincible summer", "In the middle of winter, I finally learned there was in me an invincible summer", "Amidst the winter, I discovered that within me dwelled an unconquerable summer", "All of these preserve the literary quality"), "In the midst of winter, I found there was, within me, an invincible summer", Translation)
    ),
    
    // Version 3 - Regional variations and dialects
    List(
      Question("Translate the Argentine Spanish phrase 'El colectivo va a llegar pronto' to standard English", List("The bus will arrive soon", "Public transportation will be here shortly", "The coach is coming soon", "The transit vehicle will arrive shortly"), "The bus will arrive soon", Translation),
      Question("How would you translate the German-Swiss dialect phrase 'Grüezi mitenand' to standard English?", List("Hello everyone", "Good day to all of you", "Greetings to you all", "All of these convey the greeting"), "Hello everyone", Translation),
      Question("Translate the Quebec French phrase 'Je dois retirer de l'argent au guichet' to standard English", List("I need to withdraw money from the ATM", "I have to get cash from the cash machine", "I need to get money from the bank machine", "All of these convey the same meaning"), "I need to withdraw money from the ATM", Translation),
      Question("How would you translate the Egyptian Arabic phrase 'ازيك؟ عامل ايه؟' to standard English?", List("How are you? How's it going?", "How are you doing?", "How's everything with you?", "All of these convey the greeting"), "How are you? How's it going?", Translation),
      Question("Translate the Sicilian Italian phrase 'Unni vai?' to standard English", List("Where are you going?", "Where to?", "Where are you headed?", "All of these convey the question"), "Where are you going?", Translation)
    )
  )
}
