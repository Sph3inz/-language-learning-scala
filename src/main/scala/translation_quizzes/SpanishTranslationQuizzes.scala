package languagelearningbot.translation_quizzes

import languagelearningbot._

/**
 * Spanish Translation Quizzes
 * Contains translation quiz questions for Spanish in all difficulty levels
 */
object SpanishTranslationQuizzes {
  
  // Get Spanish translation quizzes for a specific difficulty level
  def getQuizzes(difficulty: Difficulty): List[List[Question]] = {
    difficulty match {
      case Easy => easyQuizzes
      case Medium => mediumQuizzes
      case Hard => hardQuizzes
      case Impossible => impossibleQuizzes
    }
  }
  
  // Spanish translation quizzes - Easy difficulty (3 versions)
  private val easyQuizzes: List[List[Question]] = List(
    // Version 1 - Basic greetings and phrases
    List(
      Question("How do you say 'Hello, how are you?' in Spanish?", List("Hola, ¿cómo estás?", "Adiós, ¿qué tal?", "Buenos días, ¿dónde estás?", "Hola, ¿quién eres?"), "Hola, ¿cómo estás?", Translation),
      Question("Translate 'My name is John' to Spanish", List("Me llamo John", "Mi nombre es John", "Soy John", "All of these are correct"), "All of these are correct", Translation),
      Question("Translate 'I would like a coffee, please' to Spanish", List("Quisiera un café, por favor", "Quiero un café, gracias", "Necesito un café, por favor", "Dame un café, por favor"), "Quisiera un café, por favor", Translation),
      Question("How do you say 'Where is the bathroom?' in Spanish?", List("¿Dónde está el baño?", "¿Dónde es el baño?", "¿Cuál es el baño?", "¿Qué es el baño?"), "¿Dónde está el baño?", Translation),
      Question("Translate 'Thank you very much' to Spanish", List("Muchas gracias", "Gracias mucho", "Muy gracias", "Gracias muy"), "Muchas gracias", Translation)
    ),
    
    // Version 2 - Food and restaurants
    List(
      Question("Translate 'I would like to order' to Spanish", List("Quisiera ordenar", "Quiero pedir", "Me gustaría pedir", "All of these are correct"), "All of these are correct", Translation),
      Question("How do you say 'The bill, please' in Spanish?", List("La cuenta, por favor", "El precio, por favor", "La factura, por favor", "El pago, por favor"), "La cuenta, por favor", Translation),
      Question("Translate 'Is there a vegetarian option?' to Spanish", List("¿Hay alguna opción vegetariana?", "¿Existe comida vegetariana?", "¿Tienen platos sin carne?", "¿Puedo comer vegetariano aquí?"), "¿Hay alguna opción vegetariana?", Translation),
      Question("How do you say 'This food is delicious' in Spanish?", List("Esta comida está deliciosa", "Esta comida es deliciosa", "Esta comida sabe deliciosa", "All of these are correct"), "Esta comida está deliciosa", Translation),
      Question("Translate 'I am allergic to nuts' to Spanish", List("Soy alérgico a los frutos secos", "Tengo alergia a las nueces", "No puedo comer nueces", "All of these are correct"), "Soy alérgico a los frutos secos", Translation)
    ),
    
    // Version 3 - Travel and directions
    List(
      Question("How do you say 'How much does this cost?' in Spanish?", List("¿Cuánto cuesta esto?", "¿Cuál es el precio?", "¿Cuánto es?", "All of these are correct"), "All of these are correct", Translation),
      Question("Translate 'I need a hotel' to Spanish", List("Necesito un hotel", "Quiero un hotel", "Busco un hotel", "All of these are correct"), "All of these are correct", Translation),
      Question("How do you say 'Turn right at the corner' in Spanish?", List("Gire a la derecha en la esquina", "Doble a la derecha en la esquina", "Vaya a la derecha en la esquina", "All of these are correct"), "All of these are correct", Translation),
      Question("Translate 'Is it far from here?' to Spanish", List("¿Está lejos de aquí?", "¿Es lejos de aquí?", "¿Queda lejos de aquí?", "All of these are correct"), "All of these are correct", Translation),
      Question("How do you say 'I am lost' in Spanish?", List("Estoy perdido", "Me he perdido", "No sé dónde estoy", "All of these are correct"), "All of these are correct", Translation)
    )
  )
  
  // Spanish translation quizzes - Medium difficulty (3 versions)
  private val mediumQuizzes: List[List[Question]] = List(
    // Version 1 - Everyday situations
    List(
      Question("Translate 'I've been waiting for 20 minutes' to Spanish", List("He estado esperando durante 20 minutos", "Estoy esperando por 20 minutos", "Espero desde hace 20 minutos", "Llevo 20 minutos esperando"), "Llevo 20 minutos esperando", Translation),
      Question("How would you say 'Could you speak more slowly, please?' in Spanish?", List("¿Podría hablar más despacio, por favor?", "¿Puede hablar más lento, por favor?", "¿Hablaría más despacio, por favor?", "¿Quiere hablar más despacio, por favor?"), "¿Podría hablar más despacio, por favor?", Translation),
      Question("Translate 'I don't understand what you're saying' to Spanish", List("No entiendo lo que dices", "No comprendo lo que hablas", "No sé qué estás diciendo", "No entiendo tus palabras"), "No entiendo lo que dices", Translation),
      Question("How would you translate 'We should meet tomorrow' to Spanish?", List("Deberíamos reunirnos mañana", "Tenemos que vernos mañana", "Hay que encontrarnos mañana", "Vamos a juntarnos mañana"), "Deberíamos reunirnos mañana", Translation),
      Question("Translate 'What time does the store open?' to Spanish", List("¿A qué hora abre la tienda?", "¿Cuándo abre la tienda?", "¿Qué hora abre la tienda?", "¿En qué momento abre la tienda?"), "¿A qué hora abre la tienda?", Translation)
    ),
    
    // Version 2 - Health and emergencies
    List(
      Question("How would you say 'I need to see a doctor' in Spanish?", List("Necesito ver a un médico", "Tengo que ir al doctor", "Debo consultar con un médico", "Quiero ver a un doctor"), "Necesito ver a un médico", Translation),
      Question("Translate 'I have a headache' to Spanish", List("Tengo dolor de cabeza", "Me duele la cabeza", "Tengo una jaqueca", "All of these are correct"), "All of these are correct", Translation),
      Question("How would you say 'Where is the nearest pharmacy?' in Spanish?", List("¿Dónde está la farmacia más cercana?", "¿Cuál es la farmacia más próxima?", "¿Hay alguna farmacia cerca?", "¿Puede indicarme una farmacia cercana?"), "¿Dónde está la farmacia más cercana?", Translation),
      Question("Translate 'I've lost my passport' to Spanish", List("He perdido mi pasaporte", "No encuentro mi pasaporte", "Mi pasaporte se ha extraviado", "No tengo mi pasaporte"), "He perdido mi pasaporte", Translation),
      Question("How would you say 'Call an ambulance!' in Spanish?", List("¡Llame a una ambulancia!", "¡Necesito una ambulancia!", "¡Por favor, una ambulancia!", "All of these are acceptable"), "¡Llame a una ambulancia!", Translation)
    ),
    
    // Version 3 - Work and business
    List(
      Question("Translate 'I would like to schedule a meeting' to Spanish", List("Me gustaría programar una reunión", "Quisiera agendar una reunión", "Deseo concertar una reunión", "All of these are correct"), "All of these are correct", Translation),
      Question("How would you say 'Please send me the documents by email' in Spanish?", List("Por favor, envíeme los documentos por correo electrónico", "Mándeme los documentos por email, por favor", "Necesito los documentos por correo, por favor", "All of these are acceptable"), "Por favor, envíeme los documentos por correo electrónico", Translation),
      Question("Translate 'We need to discuss this project' to Spanish", List("Necesitamos discutir este proyecto", "Tenemos que hablar sobre este proyecto", "Debemos conversar acerca de este proyecto", "All of these are correct"), "All of these are correct", Translation),
      Question("How would you say 'I'm interested in your proposal' in Spanish?", List("Estoy interesado en su propuesta", "Me interesa su propuesta", "Su propuesta me parece interesante", "All of these are correct"), "All of these are correct", Translation),
      Question("Translate 'Could you explain that again?' to Spanish", List("¿Podría explicar eso de nuevo?", "¿Puede repetir la explicación?", "¿Me lo explicaría otra vez?", "All of these are correct"), "All of these are correct", Translation)
    )
  )
  
  // Spanish translation quizzes - Hard difficulty (3 versions)
  private val hardQuizzes: List[List[Question]] = List(
    // Version 1 - Complex social interactions
    List(
      Question("Translate 'I'd appreciate it if you could help me with this matter' to Spanish", List("Agradecería que me pudiera ayudar con este asunto", "Le estaría agradecido si me ayudase con este tema", "Valoraría mucho su ayuda en este asunto", "All of these are acceptable"), "Agradecería que me pudiera ayudar con este asunto", Translation),
      Question("How would you translate 'We regret to inform you that your application has been rejected' to Spanish?", List("Lamentamos informarle que su solicitud ha sido rechazada", "Sentimos comunicarle que su aplicación ha sido denegada", "Nos pesa informarle que su solicitud no ha sido aceptada", "All of these convey the same message"), "Lamentamos informarle que su solicitud ha sido rechazada", Translation),
      Question("Translate 'It's not what you said, but how you said it' to Spanish", List("No es lo que dijiste, sino cómo lo dijiste", "No es qué has dicho, sino cómo lo has dicho", "No se trata de tus palabras, sino de tu tono", "No es el contenido sino la forma"), "No es lo que dijiste, sino cómo lo dijiste", Translation),
      Question("How would you say 'Let's agree to disagree' in Spanish?", List("Acordemos estar en desacuerdo", "Estemos de acuerdo en que no estamos de acuerdo", "Aceptemos que tenemos opiniones diferentes", "All of these are acceptable"), "Acordemos estar en desacuerdo", Translation),
      Question("Translate 'I didn't mean to offend you' to Spanish", List("No era mi intención ofenderte", "No quise ofenderte", "No pretendía molestarte", "All of these are correct"), "All of these are correct", Translation)
    ),
    
    // Version 2 - Cultural and idiomatic expressions
    List(
      Question("How would you translate the idiomatic expression 'It's raining cats and dogs' to Spanish?", List("Está lloviendo a cántaros", "Llueve a mares", "Está diluviando", "All of these convey the same meaning"), "All of these convey the same meaning", Translation),
      Question("Translate 'You can't have your cake and eat it too' to Spanish", List("No puedes tener todo en la vida", "No se puede estar en misa y repicando", "Querer estar en dos sitios a la vez", "All of these convey a similar meaning"), "No se puede estar en misa y repicando", Translation),
      Question("How would you say 'That's the last straw' in Spanish?", List("Eso es la gota que colma el vaso", "Es la última paja", "Es el colmo", "Ya no aguanto más"), "Eso es la gota que colma el vaso", Translation),
      Question("Translate 'To beat around the bush' to Spanish", List("Andarse con rodeos", "Irse por las ramas", "No ir al grano", "All of these convey the same meaning"), "All of these convey the same meaning", Translation),
      Question("How would you translate 'When pigs fly' to Spanish?", List("Cuando las ranas críen pelo", "Cuando los cerdos vuelen", "Cuando los burros vuelen", "All of these convey the same meaning"), "Cuando las ranas críen pelo", Translation)
    ),
    
    // Version 3 - Abstract concepts
    List(
      Question("Translate 'The meaning of life is a philosophical question that has been debated throughout history' to Spanish", List("El significado de la vida es una cuestión filosófica que ha sido debatida a lo largo de la historia", "El sentido de la vida es una pregunta filosófica discutida a través de la historia", "El propósito de la vida es un interrogante filosófico debatido históricamente", "Todas estas traducciones son aceptables"), "El significado de la vida es una cuestión filosófica que ha sido debatida a lo largo de la historia", Translation),
      Question("How would you translate 'Consciousness remains one of the biggest mysteries in science' to Spanish?", List("La conciencia sigue siendo uno de los mayores misterios de la ciencia", "La consciencia continúa siendo uno de los enigmas más grandes de la ciencia", "La conciencia permanece como uno de los misterios más grandes en la ciencia", "All of these are acceptable"), "La conciencia sigue siendo uno de los mayores misterios de la ciencia", Translation),
      Question("Translate 'Sustainable development aims to meet human needs while preserving the environment' to Spanish", List("El desarrollo sostenible busca satisfacer las necesidades humanas mientras preserva el medio ambiente", "El desarrollo sustentable tiene como objetivo cubrir las necesidades humanas a la vez que conserva el entorno", "El desarrollo sostenible pretende atender las necesidades humanas preservando el ambiente", "All of these convey the same meaning"), "El desarrollo sostenible busca satisfacer las necesidades humanas mientras preserva el medio ambiente", Translation),
      Question("How would you say 'Democracy is based on the principle of rule by the people' in Spanish?", List("La democracia se basa en el principio del gobierno por el pueblo", "La democracia está fundada en el principio de la gobernanza popular", "La democracia tiene como fundamento el principio del gobierno del pueblo", "All of these are acceptable"), "La democracia se basa en el principio del gobierno por el pueblo", Translation),
      Question("Translate 'The concept of justice varies across different cultures and historical periods' to Spanish", List("El concepto de justicia varía entre diferentes culturas y períodos históricos", "La noción de justicia cambia según distintas culturas y épocas históricas", "La idea de justicia es distinta en diferentes culturas y momentos históricos", "All of these convey the same meaning"), "El concepto de justicia varía entre diferentes culturas y períodos históricos", Translation)
    )
  )
  
  // Spanish translation quizzes - Impossible difficulty (3 versions)
  private val impossibleQuizzes: List[List[Question]] = List(
    // Version 1 - Technical and specialized language
    List(
      Question("Translate the legal term 'The burden of proof rests with the prosecution' to Spanish", List("La carga de la prueba recae sobre la fiscalía", "El peso de la prueba corresponde a la acusación", "La obligación probatoria pertenece al ministerio fiscal", "All of these convey the correct legal meaning"), "La carga de la prueba recae sobre la fiscalía", Translation),
      Question("How would you translate the medical term 'The patient presents with acute myocardial infarction requiring immediate intervention' to Spanish?", List("El paciente presenta un infarto agudo de miocardio que requiere intervención inmediata", "El paciente muestra un infarto de miocardio agudo que necesita intervención inmediata", "El paciente sufre un infarto agudo del miocardio que requiere tratamiento inmediato", "All of these convey the medical meaning"), "El paciente presenta un infarto agudo de miocardio que requiere intervención inmediata", Translation),
      Question("Translate the technical computing term 'The algorithm optimizes memory allocation through dynamic garbage collection' to Spanish", List("El algoritmo optimiza la asignación de memoria mediante recolección dinámica de basura", "El algoritmo mejora la asignación de memoria a través de la recolección dinámica de residuos", "El algoritmo eficientiza la distribución de memoria por medio de la recolección dinámica de desechos", "All of these express the technical concept"), "El algoritmo optimiza la asignación de memoria mediante recolección dinámica de basura", Translation),
      Question("How would you translate the financial term 'The leveraged buyout resulted in significant corporate restructuring' to Spanish?", List("La adquisición apalancada resultó en una reestructuración corporativa significativa", "La compra con apalancamiento dio lugar a una importante reestructuración empresarial", "La adquisición mediante endeudamiento provocó una restructuración corporativa sustancial", "All of these convey the financial concept"), "La adquisición apalancada resultó en una reestructuración corporativa significativa", Translation),
      Question("Translate the philosophical concept 'Existentialism posits that individuals create meaning in an otherwise meaningless universe' to Spanish", List("El existencialismo postula que los individuos crean significado en un universo que de otro modo carecería de sentido", "El existencialismo propone que las personas generan sentido en un universo intrínsecamente carente de significado", "El existencialismo afirma que los seres humanos dotan de significado a un universo inherentemente sin sentido", "All of these express the philosophical concept"), "El existencialismo postula que los individuos crean significado en un universo que de otro modo carecería de sentido", Translation)
    ),
    
    // Version 2 - Literary and poetic language
    List(
      Question("Translate this line from Neruda: 'Tonight I can write the saddest lines' to its original Spanish", List("Esta noche puedo escribir los versos más tristes", "Puedo escribir los versos más tristes esta noche", "Esta noche escribiré los versos más tristes", "Hoy puedo escribir los versos más tristes"), "Puedo escribir los versos más tristes esta noche", Translation),
      Question("How would you translate Shakespeare's 'To be or not to be, that is the question' to Spanish preserving its poetic quality?", List("Ser o no ser, esa es la cuestión", "Existir o no existir, esa es la pregunta", "Ser o no ser, he ahí el dilema", "Vivir o no vivir, esa es la duda"), "Ser o no ser, esa es la cuestión", Translation),
      Question("Translate the metaphorical expression 'Time is a thief that leaves no fingerprints' to Spanish", List("El tiempo es un ladrón que no deja huellas", "El tiempo es un ladrón sin huellas dactilares", "El tiempo roba sin dejar rastro", "All of these preserve the metaphor"), "El tiempo es un ladrón que no deja huellas", Translation),
      Question("How would you translate the poetic phrase 'The whispers of autumn leaves dancing in the wind' to Spanish?", List("Los susurros de las hojas otoñales bailando en el viento", "El murmullo de las hojas de otoño danzando con el viento", "Los suaves rumores de las hojas otoñales que danzan al viento", "All of these preserve the poetic quality"), "Los susurros de las hojas otoñales bailando en el viento", Translation),
      Question("Translate the literary quote 'In the midst of winter, I found there was, within me, an invincible summer' to Spanish", List("En medio del invierno, encontré que había, dentro de mí, un verano invencible", "En pleno invierno, descubrí que había, en mi interior, un verano invencible", "En el corazón del invierno, hallé que existía, dentro de mí, un verano invencible", "All of these preserve the literary quality"), "En medio del invierno, encontré que había, dentro de mí, un verano invencible", Translation)
    ),
    
    // Version 3 - Regional variations and dialects
    List(
      Question("Translate 'The bus will arrive soon' using Argentine Spanish specifically", List("El colectivo va a llegar pronto", "El bondi está por llegar", "El micro llegará en breve", "El ómnibus arribará pronto"), "El colectivo va a llegar pronto", Translation),
      Question("How would you say 'That's really cool!' in Mexican Spanish slang?", List("¡Está bien padre!", "¡Qué chido!", "¡Está súper chingón!", "All of these are used in Mexican Spanish"), "All of these are used in Mexican Spanish", Translation),
      Question("Translate 'I need to get some money from the ATM' using specifically Spanish from Spain", List("Necesito sacar dinero del cajero", "Tengo que sacar pasta del cajero automático", "He de sacar dinero del banco automático", "Debo obtener euros del cajero"), "Necesito sacar dinero del cajero", Translation),
      Question("How would you say 'The car broke down' in Cuban Spanish?", List("La máquina se rompió", "El carro se descompuso", "El auto se averió", "El coche se estropeó"), "La máquina se rompió", Translation),
      Question("Translate 'We're going to have a party this weekend' using Colombian Spanish", List("Vamos a hacer una rumba este fin de semana", "Haremos una fiesta este fin de semana", "Tendremos un parche este finde", "Vamos a parrandear este fin de semana"), "Vamos a hacer una rumba este fin de semana", Translation)
    )
  )
}
