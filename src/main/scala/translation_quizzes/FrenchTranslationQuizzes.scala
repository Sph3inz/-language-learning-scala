package languagelearningbot.translation_quizzes

import languagelearningbot._

/**
 * French Translation Quizzes
 * Contains translation quiz questions for French in all difficulty levels
 */
object FrenchTranslationQuizzes {
  
  // Get French translation quizzes for a specific difficulty level
  def getQuizzes(difficulty: Difficulty): List[List[Question]] = {
    difficulty match {
      case Easy => easyQuizzes
      case Medium => mediumQuizzes
      case Hard => hardQuizzes
      case Impossible => impossibleQuizzes
    }
  }
  
  // French translation quizzes - Easy difficulty (3 versions)
  private val easyQuizzes: List[List[Question]] = List(
    // Version 1 - Basic greetings and phrases
    List(
      Question("How do you say 'Hello, how are you?' in French?", List("Bonjour, comment allez-vous?", "Salut, comment vas-tu?", "Bonjour, comment ça va?", "All of these are correct"), "All of these are correct", Translation),
      Question("Translate 'My name is John' to French", List("Je m'appelle John", "Mon nom est John", "Je suis John", "All of these are acceptable"), "Je m'appelle John", Translation),
      Question("Translate 'I would like a coffee, please' to French", List("Je voudrais un café, s'il vous plaît", "Je veux un café, merci", "J'ai besoin d'un café, s'il vous plaît", "Donnez-moi un café, s'il vous plaît"), "Je voudrais un café, s'il vous plaît", Translation),
      Question("How do you say 'Where is the bathroom?' in French?", List("Où sont les toilettes?", "Où est la salle de bain?", "Où se trouve la toilette?", "All of these are acceptable"), "Où sont les toilettes?", Translation),
      Question("Translate 'Thank you very much' to French", List("Merci beaucoup", "Merci bien", "Je vous remercie", "All of these are correct"), "Merci beaucoup", Translation)
    ),
    
    // Version 2 - Food and restaurants
    List(
      Question("Translate 'I would like to order' to French", List("Je voudrais commander", "Je veux commander", "J'aimerais commander", "All of these are correct"), "All of these are correct", Translation),
      Question("How do you say 'The bill, please' in French?", List("L'addition, s'il vous plaît", "La facture, s'il vous plaît", "Le compte, s'il vous plaît", "La note, s'il vous plaît"), "L'addition, s'il vous plaît", Translation),
      Question("Translate 'Is there a vegetarian option?' to French", List("Y a-t-il une option végétarienne?", "Avez-vous des plats végétariens?", "Est-ce qu'il y a quelque chose pour les végétariens?", "All of these are acceptable"), "Y a-t-il une option végétarienne?", Translation),
      Question("How do you say 'This food is delicious' in French?", List("Cette nourriture est délicieuse", "Ce plat est délicieux", "C'est délicieux", "All of these are correct"), "All of these are correct", Translation),
      Question("Translate 'I am allergic to nuts' to French", List("Je suis allergique aux noix", "J'ai une allergie aux fruits à coque", "Je ne peux pas manger de noix", "All of these convey the same meaning"), "Je suis allergique aux noix", Translation)
    ),
    
    // Version 3 - Travel and directions
    List(
      Question("How do you say 'How much does this cost?' in French?", List("Combien ça coûte?", "Quel est le prix?", "C'est combien?", "All of these are correct"), "All of these are correct", Translation),
      Question("Translate 'I need a hotel' to French", List("J'ai besoin d'un hôtel", "Je cherche un hôtel", "Il me faut un hôtel", "All of these are correct"), "All of these are correct", Translation),
      Question("How do you say 'Turn right at the corner' in French?", List("Tournez à droite au coin", "Prenez à droite au coin", "À droite au coin", "All of these are acceptable"), "Tournez à droite au coin", Translation),
      Question("Translate 'Is it far from here?' to French", List("Est-ce loin d'ici?", "C'est loin d'ici?", "Est-ce que c'est loin?", "All of these are correct"), "All of these are correct", Translation),
      Question("How do you say 'I am lost' in French?", List("Je suis perdu(e)", "Je me suis égaré(e)", "Je ne sais pas où je suis", "All of these are correct"), "All of these are correct", Translation)
    )
  )
  
  // French translation quizzes - Medium difficulty (3 versions)
  private val mediumQuizzes: List[List[Question]] = List(
    // Version 1 - Everyday situations
    List(
      Question("Translate 'I've been waiting for 20 minutes' to French", List("J'attends depuis 20 minutes", "Ça fait 20 minutes que j'attends", "J'ai attendu pendant 20 minutes", "Je suis en train d'attendre depuis 20 minutes"), "J'attends depuis 20 minutes", Translation),
      Question("How would you say 'Could you speak more slowly, please?' in French?", List("Pourriez-vous parler plus lentement, s'il vous plaît?", "Pouvez-vous parler plus lentement, s'il vous plaît?", "Parlez plus lentement, s'il vous plaît", "Est-ce que vous pouvez ralentir, s'il vous plaît?"), "Pourriez-vous parler plus lentement, s'il vous plaît?", Translation),
      Question("Translate 'I don't understand what you're saying' to French", List("Je ne comprends pas ce que vous dites", "Je ne comprends pas ce que tu dis", "Je ne saisis pas vos paroles", "All of these are correct depending on formality"), "All of these are correct depending on formality", Translation),
      Question("How would you translate 'We should meet tomorrow' to French?", List("Nous devrions nous rencontrer demain", "On devrait se voir demain", "Il faudrait qu'on se retrouve demain", "All of these are correct"), "All of these are correct", Translation),
      Question("Translate 'What time does the store open?' to French", List("À quelle heure ouvre le magasin?", "À quelle heure le magasin ouvre-t-il?", "Quand est-ce que le magasin ouvre?", "All of these are correct"), "All of these are correct", Translation)
    ),
    
    // Version 2 - Health and emergencies
    List(
      Question("How would you say 'I need to see a doctor' in French?", List("J'ai besoin de voir un médecin", "Je dois consulter un médecin", "Il me faut voir un docteur", "All of these are correct"), "All of these are correct", Translation),
      Question("Translate 'I have a headache' to French", List("J'ai mal à la tête", "J'ai une migraine", "Ma tête me fait mal", "All of these convey the same meaning"), "J'ai mal à la tête", Translation),
      Question("How would you say 'Where is the nearest pharmacy?' in French?", List("Où est la pharmacie la plus proche?", "Où se trouve la pharmacie la plus proche?", "Y a-t-il une pharmacie près d'ici?", "All of these are acceptable"), "Où est la pharmacie la plus proche?", Translation),
      Question("Translate 'I've lost my passport' to French", List("J'ai perdu mon passeport", "Je ne trouve pas mon passeport", "Mon passeport a disparu", "All of these convey the same meaning"), "J'ai perdu mon passeport", Translation),
      Question("How would you say 'Call an ambulance!' in French?", List("Appelez une ambulance!", "Téléphonez à une ambulance!", "Il faut une ambulance!", "All of these are acceptable in an emergency"), "Appelez une ambulance!", Translation)
    ),
    
    // Version 3 - Work and business
    List(
      Question("Translate 'I would like to schedule a meeting' to French", List("Je voudrais organiser une réunion", "J'aimerais programmer une réunion", "Je souhaite planifier une réunion", "All of these are correct"), "All of these are correct", Translation),
      Question("How would you say 'Please send me the documents by email' in French?", List("Veuillez m'envoyer les documents par email", "Envoyez-moi les documents par courriel, s'il vous plaît", "Merci de m'envoyer les documents par mail", "All of these are acceptable"), "Veuillez m'envoyer les documents par email", Translation),
      Question("Translate 'We need to discuss this project' to French", List("Nous devons discuter de ce projet", "Il faut qu'on parle de ce projet", "On doit aborder ce projet", "All of these are correct"), "Nous devons discuter de ce projet", Translation),
      Question("How would you say 'I'm interested in your proposal' in French?", List("Je suis intéressé(e) par votre proposition", "Votre proposition m'intéresse", "J'ai un intérêt pour votre proposition", "All of these convey the same meaning"), "Je suis intéressé(e) par votre proposition", Translation),
      Question("Translate 'Could you explain that again?' to French", List("Pourriez-vous expliquer cela à nouveau?", "Pouvez-vous répéter l'explication?", "Pourriez-vous me réexpliquer?", "All of these are acceptable"), "Pourriez-vous expliquer cela à nouveau?", Translation)
    )
  )
  
  // French translation quizzes - Hard difficulty (3 versions)
  private val hardQuizzes: List[List[Question]] = List(
    // Version 1 - Complex social interactions
    List(
      Question("Translate 'I'd appreciate it if you could help me with this matter' to French", List("J'apprécierais que vous puissiez m'aider dans cette affaire", "Je vous serais reconnaissant(e) de m'aider avec cette question", "Je vous saurais gré de bien vouloir m'assister dans cette affaire", "All of these are formal and correct"), "All of these are formal and correct", Translation),
      Question("How would you translate 'We regret to inform you that your application has been rejected' to French?", List("Nous regrettons de vous informer que votre candidature a été rejetée", "Nous sommes au regret de vous informer que votre demande n'a pas été retenue", "Nous avons le regret de vous annoncer que votre dossier a été refusé", "All of these convey the formal message"), "Nous regrettons de vous informer que votre candidature a été rejetée", Translation),
      Question("Translate 'It's not what you said, but how you said it' to French", List("Ce n'est pas ce que vous avez dit, mais comment vous l'avez dit", "Ce n'est pas le contenu mais la forme de vos propos", "Ce n'est pas vos paroles qui posent problème, mais votre ton", "All of these convey the same meaning"), "Ce n'est pas ce que vous avez dit, mais comment vous l'avez dit", Translation),
      Question("How would you say 'Let's agree to disagree' in French?", List("Convenons d'être en désaccord", "Acceptons que nos opinions diffèrent", "Nous sommes d'accord pour ne pas être d'accord", "All of these express the same idea"), "Convenons d'être en désaccord", Translation),
      Question("Translate 'I didn't mean to offend you' to French", List("Je ne voulais pas vous offenser", "Ce n'était pas mon intention de vous blesser", "Je ne cherchais pas à vous vexer", "All of these are correct"), "All of these are correct", Translation)
    ),
    
    // Version 2 - Cultural and idiomatic expressions
    List(
      Question("How would you translate the idiomatic expression 'It's raining cats and dogs' to French?", List("Il pleut des cordes", "Il pleut à verse", "Il pleut comme vache qui pisse", "All of these convey the same meaning"), "Il pleut des cordes", Translation),
      Question("Translate 'You can't have your cake and eat it too' to French", List("On ne peut pas avoir le beurre et l'argent du beurre", "Il faut choisir: ou l'un ou l'autre", "On ne peut pas tout avoir", "All of these convey the idiomatic meaning"), "On ne peut pas avoir le beurre et l'argent du beurre", Translation),
      Question("How would you say 'That's the last straw' in French?", List("C'est la goutte d'eau qui fait déborder le vase", "C'est la dernière paille", "C'est la cerise sur le gâteau", "C'est le bouquet"), "C'est la goutte d'eau qui fait déborder le vase", Translation),
      Question("Translate 'To beat around the bush' to French", List("Tourner autour du pot", "Ne pas aller droit au but", "Prendre des chemins détournés", "All of these express the same idea"), "Tourner autour du pot", Translation),
      Question("How would you translate 'When pigs fly' to French?", List("Quand les poules auront des dents", "Quand les cochons voleront", "À la Saint-Glinglin", "All of these express impossibility"), "Quand les poules auront des dents", Translation)
    ),
    
    // Version 3 - Abstract concepts
    List(
      Question("Translate 'The meaning of life is a philosophical question that has been debated throughout history' to French", List("Le sens de la vie est une question philosophique qui a été débattue tout au long de l'histoire", "La signification de la vie est une interrogation philosophique débattue à travers l'histoire", "Le but de l'existence est une problématique philosophique discutée depuis toujours", "All of these convey the abstract concept"), "Le sens de la vie est une question philosophique qui a été débattue tout au long de l'histoire", Translation),
      Question("How would you translate 'Consciousness remains one of the biggest mysteries in science' to French?", List("La conscience demeure l'un des plus grands mystères de la science", "La conscience reste l'une des plus grandes énigmes scientifiques", "La conscience constitue encore l'un des mystères majeurs de la science", "All of these express the concept correctly"), "La conscience demeure l'un des plus grands mystères de la science", Translation),
      Question("Translate 'Sustainable development aims to meet human needs while preserving the environment' to French", List("Le développement durable vise à répondre aux besoins humains tout en préservant l'environnement", "Le développement soutenable cherche à satisfaire les besoins humains en conservant l'environnement", "Le développement durable a pour objectif de combler les besoins humains en protégeant l'environnement", "All of these convey the concept"), "Le développement durable vise à répondre aux besoins humains tout en préservant l'environnement", Translation),
      Question("How would you say 'Democracy is based on the principle of rule by the people' in French?", List("La démocratie est fondée sur le principe du gouvernement par le peuple", "La démocratie repose sur le principe de la gouvernance populaire", "La démocratie se base sur le principe du pouvoir exercé par le peuple", "All of these express the concept"), "La démocratie est fondée sur le principe du gouvernement par le peuple", Translation),
      Question("Translate 'The concept of justice varies across different cultures and historical periods' to French", List("Le concept de justice varie selon les différentes cultures et périodes historiques", "La notion de justice diffère entre les cultures et les époques historiques", "L'idée de justice change d'une culture à l'autre et d'une période historique à l'autre", "All of these convey the abstract concept"), "Le concept de justice varie selon les différentes cultures et périodes historiques", Translation)
    )
  )
  
  // French translation quizzes - Impossible difficulty (3 versions)
  private val impossibleQuizzes: List[List[Question]] = List(
    // Version 1 - Technical and specialized language
    List(
      Question("Translate the legal term 'The burden of proof rests with the prosecution' to French", List("La charge de la preuve incombe à l'accusation", "Le fardeau de la preuve repose sur le ministère public", "L'obligation de prouver revient à la partie poursuivante", "All of these convey the legal concept"), "La charge de la preuve incombe à l'accusation", Translation),
      Question("How would you translate the medical term 'The patient presents with acute myocardial infarction requiring immediate intervention' to French?", List("Le patient présente un infarctus aigu du myocarde nécessitant une intervention immédiate", "Le patient souffre d'un infarctus du myocarde aigu qui requiert une intervention immédiate", "Le patient est atteint d'un infarctus aigu du myocarde exigeant une prise en charge immédiate", "All of these express the medical terminology"), "Le patient présente un infarctus aigu du myocarde nécessitant une intervention immédiate", Translation),
      Question("Translate the technical computing term 'The algorithm optimizes memory allocation through dynamic garbage collection' to French", List("L'algorithme optimise l'allocation de mémoire par le biais d'une collecte dynamique des déchets", "L'algorithme optimise l'allocation mémoire grâce à un ramasse-miettes dynamique", "L'algorithme améliore l'attribution de la mémoire via un nettoyage dynamique de la mémoire", "All of these express the technical concept"), "L'algorithme optimise l'allocation de mémoire par le biais d'une collecte dynamique des déchets", Translation),
      Question("How would you translate the financial term 'The leveraged buyout resulted in significant corporate restructuring' to French?", List("Le rachat par emprunt a entraîné une restructuration significative de l'entreprise", "L'acquisition par effet de levier a abouti à une restructuration importante de la société", "Le LBO a conduit à une réorganisation substantielle de l'entreprise", "All of these convey the financial concept"), "Le rachat par emprunt a entraîné une restructuration significative de l'entreprise", Translation),
      Question("Translate the philosophical concept 'Existentialism posits that individuals create meaning in an otherwise meaningless universe' to French", List("L'existentialisme postule que les individus créent du sens dans un univers autrement dépourvu de signification", "L'existentialisme affirme que les êtres humains donnent un sens à un univers par ailleurs dénué de signification", "L'existentialisme soutient que chacun crée sa propre signification dans un univers fondamentalement absurde", "All of these express the philosophical concept"), "L'existentialisme postule que les individus créent du sens dans un univers autrement dépourvu de signification", Translation)
    ),
    
    // Version 2 - Literary and poetic language
    List(
      Question("Translate this famous line by Victor Hugo: 'To love another person is to see the face of God' to its original French", List("Aimer un autre être, c'est voir la face de Dieu", "Aimer autrui c'est voir le visage de Dieu", "Aimer un autre être humain, c'est contempler le visage de Dieu", "All of these capture the poetic essence"), "Aimer un autre être, c'est voir la face de Dieu", Translation),
      Question("How would you translate Shakespeare's 'To be or not to be, that is the question' to French preserving its poetic quality?", List("Être ou ne pas être, telle est la question", "Être ou n'être point, voilà la question", "Exister ou non, c'est là la question", "All of these preserve the poetic quality"), "Être ou ne pas être, telle est la question", Translation),
      Question("Translate the metaphorical expression 'Time is a thief that leaves no fingerprints' to French", List("Le temps est un voleur qui ne laisse pas d'empreintes", "Le temps dérobe sans laisser de traces", "Le temps est un larron sans empreintes", "All of these preserve the metaphor"), "Le temps est un voleur qui ne laisse pas d'empreintes", Translation),
      Question("How would you translate the poetic phrase 'The whispers of autumn leaves dancing in the wind' to French?", List("Les murmures des feuilles d'automne dansant dans le vent", "Les chuchotements des feuilles automnales qui valsent avec le vent", "Le doux bruissement des feuilles d'automne qui dansent au gré du vent", "All of these preserve the poetic quality"), "Les murmures des feuilles d'automne dansant dans le vent", Translation),
      Question("Translate the literary quote from Camus 'In the midst of winter, I found there was, within me, an invincible summer' to its original French", List("Au milieu de l'hiver, j'apprenais enfin qu'il y avait en moi un été invincible", "En plein hiver, je découvrais qu'il y avait en moi un été invincible", "Au cœur de l'hiver, je trouvais qu'il existait en moi un été invincible", "All of these capture the literary essence"), "Au milieu de l'hiver, j'apprenais enfin qu'il y avait en moi un été invincible", Translation)
    ),
    
    // Version 3 - Regional variations and dialects
    List(
      Question("Translate 'The car broke down' using specifically Quebec French", List("Le char est tombé en panne", "L'auto a brisé", "Le char a lâché", "La voiture est en panne"), "Le char est tombé en panne", Translation),
      Question("How would you say 'That's really cool!' in Belgian French?", List("C'est vraiment chouette!", "C'est vachement bien!", "C'est super génial!", "C'est drôlement bon!"), "C'est vraiment chouette!", Translation),
      Question("Translate 'I need to get some money from the ATM' using specifically Swiss French", List("Je dois retirer de l'argent au bancomat", "Je dois prendre de l'argent au distributeur", "Il faut que je retire du cash au guichet automatique", "Je dois sortir des sous au bancomat"), "Je dois retirer de l'argent au bancomat", Translation),
      Question("How would you say 'We're going to have breakfast' in Senegalese French?", List("On va prendre le petit déjeuner", "Nous allons déjeuner", "On va manger le déjeuner", "Nous allons petit-déjeuner"), "Nous allons déjeuner", Translation),
      Question("Translate 'Please wait a minute' using specifically Haitian French Creole", List("Tanpri, tann yon minit", "S'il vous plaît, attendez une minute", "S'il te plaît, patientez un instant", "Souple, tann ti moman"), "Tanpri, tann yon minit", Translation)
    )
  )
}
