package languagelearningbot.translation_quizzes

import languagelearningbot._

/**
 * Arabic Translation Quizzes
 * Contains translation quiz questions for Arabic in all difficulty levels
 */
object ArabicTranslationQuizzes {
  
  // Get Arabic translation quizzes for a specific difficulty level
  def getQuizzes(difficulty: Difficulty): List[List[Question]] = {
    difficulty match {
      case Easy => easyQuizzes
      case Medium => mediumQuizzes
      case Hard => hardQuizzes
      case Impossible => impossibleQuizzes
    }
  }
  
  // Arabic translation quizzes - Easy difficulty (3 versions)
  private val easyQuizzes: List[List[Question]] = List(
    // Version 1 - Basic greetings and phrases
    List(
      Question("How do you say 'Hello, how are you?' in Arabic?", List("مرحباً، كيف حالك؟", "السلام عليكم، كيف حالك؟", "أهلاً، كيف الحال؟", "All of these are correct"), "مرحباً، كيف حالك؟", Translation),
      Question("Translate 'My name is John' to Arabic", List("اسمي جون", "أنا جون", "يُدعى جون", "All of these are acceptable"), "اسمي جون", Translation),
      Question("Translate 'I would like a coffee, please' to Arabic", List("أريد قهوة من فضلك", "أود قهوة لو سمحت", "هل يمكنني الحصول على قهوة من فضلك", "All of these are correct"), "أريد قهوة من فضلك", Translation),
      Question("How do you say 'Where is the bathroom?' in Arabic?", List("أين الحمام؟", "أين المرحاض؟", "أين دورة المياه؟", "All of these are acceptable"), "أين الحمام؟", Translation),
      Question("Translate 'Thank you very much' to Arabic", List("شكراً جزيلاً", "شكراً كثيراً", "أشكرك بشدة", "All of these are correct"), "شكراً جزيلاً", Translation)
    ),
    
    // Version 2 - Food and restaurants
    List(
      Question("Translate 'I would like to order' to Arabic", List("أريد أن أطلب", "أود أن أطلب", "أرغب في الطلب", "All of these are correct"), "أريد أن أطلب", Translation),
      Question("How do you say 'The bill, please' in Arabic?", List("الحساب من فضلك", "الفاتورة لو سمحت", "أريد أن أدفع", "All of these are acceptable"), "الحساب من فضلك", Translation),
      Question("Translate 'Is there a vegetarian option?' to Arabic", List("هل هناك خيار نباتي؟", "هل توجد أطباق نباتية؟", "هل لديكم طعام للنباتيين؟", "All of these are correct"), "هل هناك خيار نباتي؟", Translation),
      Question("How do you say 'This food is delicious' in Arabic?", List("هذا الطعام لذيذ", "الطعام شهي", "وجبة لذيذة", "All of these convey the same meaning"), "هذا الطعام لذيذ", Translation),
      Question("Translate 'I am allergic to nuts' to Arabic", List("أنا أعاني من حساسية من المكسرات", "لدي حساسية من المكسرات", "أنا حساس للمكسرات", "All of these convey the meaning"), "لدي حساسية من المكسرات", Translation)
    ),
    
    // Version 3 - Travel and directions
    List(
      Question("How do you say 'How much does this cost?' in Arabic?", List("كم يكلف هذا؟", "كم سعر هذا؟", "بكم هذا؟", "All of these are correct"), "كم يكلف هذا؟", Translation),
      Question("Translate 'I need a hotel' to Arabic", List("أحتاج إلى فندق", "أريد فندقاً", "أبحث عن فندق", "All of these are correct"), "أحتاج إلى فندق", Translation),
      Question("How do you say 'Turn right at the corner' in Arabic?", List("انعطف يميناً عند الزاوية", "اتجه يميناً عند المنعطف", "خذ اليمين عند الزاوية", "All of these convey the direction"), "انعطف يميناً عند الزاوية", Translation),
      Question("Translate 'Is it far from here?' to Arabic", List("هل هو بعيد من هنا؟", "هل هذا بعيد؟", "هل المسافة طويلة من هنا؟", "All of these are correct"), "هل هو بعيد من هنا؟", Translation),
      Question("How do you say 'I am lost' in Arabic?", List("أنا تائه", "لقد ضللت الطريق", "أنا ضائع", "All of these convey being lost"), "أنا تائه", Translation)
    )
  )
  
  // Arabic translation quizzes - Medium difficulty (3 versions)
  private val mediumQuizzes: List[List[Question]] = List(
    // Version 1 - Everyday situations
    List(
      Question("Translate 'I've been waiting for 20 minutes' to Arabic", List("أنا أنتظر منذ ٢٠ دقيقة", "انتظرت لمدة عشرين دقيقة", "لقد مر عشرون دقيقة وأنا أنتظر", "All of these convey waiting time"), "أنا أنتظر منذ ٢٠ دقيقة", Translation),
      Question("How would you say 'Could you speak more slowly, please?' in Arabic?", List("هل يمكنك التحدث ببطء أكثر، من فضلك؟", "أرجوك تكلم ببطء", "تحدث ببطء أكثر لو سمحت", "All of these ask for slower speech"), "هل يمكنك التحدث ببطء أكثر، من فضلك؟", Translation),
      Question("Translate 'I don't understand what you're saying' to Arabic", List("لا أفهم ما تقوله", "أنا لا أفهم كلامك", "لا أدري ما تعنيه", "All of these express not understanding"), "لا أفهم ما تقوله", Translation),
      Question("How would you translate 'We should meet tomorrow' to Arabic?", List("يجب أن نلتقي غداً", "علينا أن نجتمع غداً", "دعنا نقابل بعضنا غداً", "All of these suggest meeting"), "يجب أن نلتقي غداً", Translation),
      Question("Translate 'What time does the store open?' to Arabic", List("في أي ساعة يفتح المتجر؟", "متى يفتح المحل؟", "أي وقت يبدأ المتجر العمل؟", "All of these ask about opening time"), "في أي ساعة يفتح المتجر؟", Translation)
    ),
    
    // Version 2 - Health and emergencies
    List(
      Question("How would you say 'I need to see a doctor' in Arabic?", List("أحتاج إلى رؤية طبيب", "أريد مقابلة طبيب", "يجب أن أرى طبيباً", "All of these express the need"), "أحتاج إلى رؤية طبيب", Translation),
      Question("Translate 'I have a headache' to Arabic", List("أعاني من صداع", "رأسي يؤلمني", "لدي ألم في الرأس", "All of these describe a headache"), "أعاني من صداع", Translation),
      Question("How would you say 'Where is the nearest pharmacy?' in Arabic?", List("أين أقرب صيدلية؟", "أين توجد صيدلية قريبة؟", "هل هناك صيدلية بالقرب من هنا؟", "All of these ask for a pharmacy"), "أين أقرب صيدلية؟", Translation),
      Question("Translate 'I've lost my passport' to Arabic", List("لقد فقدت جواز سفري", "أضعت جواز سفري", "لا أستطيع إيجاد جواز سفري", "All of these describe losing a passport"), "لقد فقدت جواز سفري", Translation),
      Question("How would you say 'Call an ambulance!' in Arabic?", List("اتصل بالإسعاف!", "استدعِ سيارة إسعاف!", "نحتاج إلى إسعاف فوراً!", "All of these call for an ambulance"), "اتصل بالإسعاف!", Translation)
    ),
    
    // Version 3 - Work and business
    List(
      Question("Translate 'I would like to schedule a meeting' to Arabic", List("أود تحديد موعد للاجتماع", "أرغب في جدولة اجتماع", "أحتاج إلى ترتيب لقاء", "All of these request scheduling"), "أود تحديد موعد للاجتماع", Translation),
      Question("How would you say 'Please send me the documents by email' in Arabic?", List("الرجاء إرسال المستندات عبر البريد الإلكتروني", "أرسل لي الوثائق بالإيميل من فضلك", "هل يمكنك إرسال المستندات لي بالبريد الإلكتروني", "All of these request documents"), "الرجاء إرسال المستندات عبر البريد الإلكتروني", Translation),
      Question("Translate 'We need to discuss this project' to Arabic", List("نحتاج إلى مناقشة هذا المشروع", "علينا أن نتحدث عن هذا المشروع", "يجب أن نناقش هذا المشروع", "All of these suggest discussion"), "نحتاج إلى مناقشة هذا المشروع", Translation),
      Question("How would you say 'I'm interested in your proposal' in Arabic?", List("أنا مهتم باقتراحك", "اقتراحك يثير اهتمامي", "أبدي اهتماماً بعرضك", "All of these express interest"), "أنا مهتم باقتراحك", Translation),
      Question("Translate 'Could you explain that again?' to Arabic", List("هل يمكنك شرح ذلك مرة أخرى؟", "أرجو أن تعيد التوضيح", "هل تستطيع التفسير مجدداً؟", "All of these ask for repetition"), "هل يمكنك شرح ذلك مرة أخرى؟", Translation)
    )
  )
  
  // Arabic translation quizzes - Hard difficulty (3 versions)
  private val hardQuizzes: List[List[Question]] = List(
    // Version 1 - Complex social interactions
    List(
      Question("Translate 'I'd appreciate it if you could help me with this matter' to Arabic", List("سأكون ممتناً إذا استطعت مساعدتي في هذه المسألة", "أقدر كثيراً لو تمكنت من مساعدتي في هذا الأمر", "سأكون شاكراً لمساعدتك لي في هذه القضية", "All of these express appreciation"), "سأكون ممتناً إذا استطعت مساعدتي في هذه المسألة", Translation),
      Question("How would you translate 'We regret to inform you that your application has been rejected' to Arabic?", List("نأسف لإبلاغك أن طلبك قد رُفض", "نعتذر لإخبارك بأن طلبك لم يُقبل", "مع الأسف، نود إعلامك برفض طلبك", "All of these express regret formally"), "نأسف لإبلاغك أن طلبك قد رُفض", Translation),
      Question("Translate 'It's not what you said, but how you said it' to Arabic", List("ليس ما قلته، بل كيف قلته", "المشكلة ليست في كلامك، بل في أسلوبك", "ليست الكلمات، بل طريقة التعبير", "All of these convey the sentiment"), "ليس ما قلته، بل كيف قلته", Translation),
      Question("How would you say 'Let's agree to disagree' in Arabic?", List("دعنا نتفق على أن نختلف", "لنتفق على اختلافنا", "يمكننا قبول اختلاف آرائنا", "All of these express the concept"), "دعنا نتفق على أن نختلف", Translation),
      Question("Translate 'I didn't mean to offend you' to Arabic", List("لم أقصد إهانتك", "لم تكن نيتي إيذاءك", "لم أرد أن أجرح مشاعرك", "All of these convey the apology"), "لم أقصد إهانتك", Translation)
    ),
    
    // Version 2 - Cultural and idiomatic expressions
    List(
      Question("How would you translate the idiomatic expression 'It's raining cats and dogs' to Arabic?", List("إنها تمطر بغزارة", "السماء تنهمر مطراً", "المطر ينزل كأفواه القرب", "All of these convey heavy rain"), "إنها تمطر بغزارة", Translation),
      Question("Translate 'You can't have your cake and eat it too' to Arabic", List("لا يمكنك أن تحصل على كل شيء", "ليس بالإمكان الجمع بين المتناقضات", "لا يُمكن إرضاء جميع الأطراف", "All of these convey the meaning"), "لا يمكنك أن تحصل على كل شيء", Translation),
      Question("How would you say 'That's the last straw' in Arabic?", List("هذه هي القشة التي قصمت ظهر البعير", "هذه نقطة التحول", "هذا ما أفاض الكيل", "All of these express the idiom"), "هذه هي القشة التي قصمت ظهر البعير", Translation),
      Question("Translate 'To beat around the bush' to Arabic", List("يدور حول الموضوع", "لا يتحدث بصراحة", "يتكلم بطريقة غير مباشرة", "All of these convey the meaning"), "يدور حول الموضوع", Translation),
      Question("How would you translate 'When pigs fly' to Arabic?", List("عندما تبيض الديكة", "في المشمش", "إذا اجتمعت الضب والحوت", "All of these express impossibility"), "عندما تبيض الديكة", Translation)
    ),
    
    // Version 3 - Abstract concepts
    List(
      Question("Translate 'The meaning of life is a philosophical question that has been debated throughout history' to Arabic", List("معنى الحياة سؤال فلسفي نوقش على مر التاريخ", "الغاية من الحياة قضية فلسفية طُرحت عبر العصور", "مغزى الحياة تساؤل فلسفي تم التباحث فيه عبر التاريخ", "All of these convey the concept"), "معنى الحياة سؤال فلسفي نوقش على مر التاريخ", Translation),
      Question("How would you translate 'Consciousness remains one of the biggest mysteries in science' to Arabic?", List("لا يزال الوعي أحد أكبر الألغاز في العلم", "الوعي ما زال يمثل أحد أعظم أسرار العلوم", "يبقى الوعي واحداً من أكبر غموض العلم", "All of these express the concept"), "لا يزال الوعي أحد أكبر الألغاز في العلم", Translation),
      Question("Translate 'Sustainable development aims to meet human needs while preserving the environment' to Arabic", List("تهدف التنمية المستدامة إلى تلبية احتياجات البشر مع الحفاظ على البيئة", "تسعى التنمية المستدامة لتحقيق متطلبات الإنسان مع صون البيئة", "ترمي التنمية المستدامة إلى إشباع حاجات الإنسان مع حماية البيئة", "All of these convey the concept"), "تهدف التنمية المستدامة إلى تلبية احتياجات البشر مع الحفاظ على البيئة", Translation),
      Question("How would you say 'Democracy is based on the principle of rule by the people' in Arabic?", List("الديمقراطية تستند على مبدأ حكم الشعب", "تقوم الديمقراطية على أساس سلطة الشعب", "تعتمد الديمقراطية على مبدأ حكم الناس لأنفسهم", "All of these express the concept"), "الديمقراطية تستند على مبدأ حكم الشعب", Translation),
      Question("Translate 'The concept of justice varies across different cultures and historical periods' to Arabic", List("يختلف مفهوم العدالة عبر الثقافات المختلفة والفترات التاريخية", "تتباين فكرة العدل بين مختلف الثقافات والعصور التاريخية", "يتنوع مفهوم العدالة بين الثقافات المتنوعة والحقب التاريخية", "All of these convey the concept"), "يختلف مفهوم العدالة عبر الثقافات المختلفة والفترات التاريخية", Translation)
    )
  )
  
  // Arabic translation quizzes - Impossible difficulty (3 versions)
  private val impossibleQuizzes: List[List[Question]] = List(
    // Version 1 - Technical and specialized language
    List(
      Question("Translate the legal term 'The burden of proof rests with the prosecution' to Arabic", List("عبء الإثبات يقع على عاتق الادعاء", "تقع مسؤولية الإثبات على النيابة", "يتحمل الادعاء عبء البرهان", "All of these convey the legal concept"), "عبء الإثبات يقع على عاتق الادعاء", Translation),
      Question("How would you translate the medical term 'The patient presents with acute myocardial infarction requiring immediate intervention' to Arabic?", List("يعاني المريض من احتشاء حاد في عضلة القلب يستلزم تدخلاً فورياً", "تظهر على المريض أعراض نوبة قلبية حادة تتطلب تدخلاً عاجلاً", "المريض مصاب بجلطة قلبية حادة تستدعي إجراءً طبياً فورياً", "All of these express the medical condition"), "يعاني المريض من احتشاء حاد في عضلة القلب يستلزم تدخلاً فورياً", Translation),
      Question("Translate the technical computing term 'The algorithm optimizes memory allocation through dynamic garbage collection' to Arabic", List("تعمل الخوارزمية على تحسين تخصيص الذاكرة من خلال جمع النفايات الديناميكي", "تقوم الخوارزمية بتحسين توزيع الذاكرة عن طريق التنظيف التلقائي للذاكرة", "تعمل الخوارزمية على تحسين تخصيص الذاكرة باستخدام آلية حذف البيانات غير المستخدمة", "All of these express the technical concept"), "تعمل الخوارزمية على تحسين تخصيص الذاكرة من خلال جمع النفايات الديناميكي", Translation),
      Question("How would you translate the financial term 'The leveraged buyout resulted in significant corporate restructuring' to Arabic?", List("أدت عملية الشراء بالرافعة المالية إلى إعادة هيكلة كبيرة للشركة", "نتج عن الاستحواذ الممول بالديون إعادة تنظيم جوهرية للمؤسسة", "تسببت صفقة الشراء المدعومة بالديون في إعادة هيكلة مهمة للشركة", "All of these convey the financial concept"), "أدت عملية الشراء بالرافعة المالية إلى إعادة هيكلة كبيرة للشركة", Translation),
      Question("Translate the philosophical concept 'Existentialism posits that individuals create meaning in an otherwise meaningless universe' to Arabic", List("تفترض الوجودية أن الأفراد يخلقون المعنى في كون يفتقر إلى المعنى", "تطرح الفلسفة الوجودية أن البشر يصنعون المعنى في عالم خالٍ من المعنى", "تؤكد الوجودية أن الإنسان يضفي معنى على وجوده في كون لا معنى له", "All of these express the philosophical concept"), "تفترض الوجودية أن الأفراد يخلقون المعنى في كون يفتقر إلى المعنى", Translation)
    ),
    
    // Version 2 - Literary and poetic language
    List(
      Question("Translate this line by Mahmoud Darwish to English: 'على هذه الأرض ما يستحق الحياة'", List("Upon this land, there is what makes life worth living", "On this earth, there is that which is worth life", "On this land, there is something worth living for", "All of these capture the poetic meaning"), "Upon this land, there is what makes life worth living", Translation),
      Question("How would you translate Shakespeare's 'To be or not to be, that is the question' to Arabic preserving its poetic quality?", List("أن تكون أو لا تكون، ذلك هو السؤال", "أن نوجد أم لا نوجد، هذا هو السؤال", "الوجود أو العدم، ذاك هو السؤال", "All of these preserve the poetic quality"), "أن تكون أو لا تكون، ذلك هو السؤال", Translation),
      Question("Translate the metaphorical expression 'Time is a thief that leaves no fingerprints' to Arabic", List("الوقت لص لا يترك بصمات", "الزمن سارق لا يخلف أثراً", "الوقت يسرق دون أن يترك أثراً", "All of these preserve the metaphor"), "الوقت لص لا يترك بصمات", Translation),
      Question("How would you translate the poetic phrase 'The whispers of autumn leaves dancing in the wind' to Arabic?", List("همس أوراق الخريف الراقصة في الريح", "همسات أوراق الخريف وهي تتراقص مع الريح", "وشوشات أوراق الخريف المتمايلة في نسمات الهواء", "All of these preserve the poetic quality"), "همس أوراق الخريف الراقصة في الريح", Translation),
      Question("Translate the literary quote 'In the midst of winter, I found there was, within me, an invincible summer' to Arabic", List("في قلب الشتاء، وجدت في داخلي صيفاً لا يُقهر", "في وسط الشتاء، اكتشفت أن بداخلي صيفاً منيعاً", "في خضم الشتاء، عثرت بداخلي على صيف لا يُهزم", "All of these preserve the literary quality"), "في قلب الشتاء، وجدت في داخلي صيفاً لا يُقهر", Translation)
    ),
    
    // Version 3 - Regional variations and dialects
    List(
      Question("Translate the Egyptian dialect phrase 'إزيك؟ عامل إيه؟' to Modern Standard Arabic", List("كيف حالك؟ ماذا تفعل؟", "كيف أنت؟ ما الذي تفعله؟", "كيف أحوالك؟ بماذا تشتغل؟", "All of these convey the greeting"), "كيف حالك؟ ماذا تفعل؟", Translation),
      Question("How would you say 'I want to go to the market' in Moroccan Arabic dialect as opposed to standard Arabic?", List("بغيت نمشي للسوق", "أريد أن أذهب إلى السوق", "أود الذهاب إلى السوق", "أرغب في الذهاب للسوق"), "بغيت نمشي للسوق", Translation),
      Question("Translate the Lebanese dialect phrase 'شو عم تعمل؟' to Modern Standard Arabic", List("ماذا تفعل؟", "ما الذي تفعله؟", "بماذا تقوم؟", "All of these ask what someone is doing"), "ماذا تفعل؟", Translation),
      Question("How would you translate the Gulf Arabic phrase 'من وين أنت؟' to Modern Standard Arabic?", List("من أين أنت؟", "من أي بلد أنت؟", "ما هو بلدك؟", "All of these ask about origin"), "من أين أنت؟", Translation),
      Question("Translate the Iraqi dialect phrase 'شلونك؟' to Modern Standard Arabic", List("كيف حالك؟", "كيف أحوالك؟", "كيف أنت؟", "All of these ask about wellbeing"), "كيف حالك؟", Translation)
    )
  )
}
