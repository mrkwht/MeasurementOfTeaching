rm(list=ls())
source("C:/Users/mrkwht/Dropbox/Research/R libraries/DefaultScriptMark.R")
# source("C:/Users/Buffning/Documents/My Dropbox/Research/R libraries/DefaultScriptMark.R")
require(gdata)
require(data.table)
require(Hmisc)
require(polycor)
require(foreign)
require(iplots)
options(digits=2)
setwd("C:/Users/mrkwht/Google Drive/Measurement of Teaching")
# setwd("C:/Users/Buffning/Documents/My Dropbox/Classes/Measurement of Teaching")
#

# Load Data ---------------------------------------------------------------

# Teacher01 <- read.spss("Teacher Questionnaire 2000-2001.sav",to.data.frame=T); label(Teacher01,self=F)<-attr(Teacher01,"variable.labels")
# Teacher02 <- read.spss("Teacher Questionnaire 2001-2002.sav",to.data.frame=T); label(Teacher02,self=F)<-attr(Teacher02,"variable.labels")
# Teacher03 <- read.spss("Teacher Questionnaire 2002-2003.sav",to.data.frame=T); label(Teacher03,self=F)<-attr(Teacher03,"variable.labels")
Teacher04 <- read.spss("data/Teacher Questionnaire 2003-2004.sav",to.data.frame=T); label(Teacher04,self=F)<-attr(Teacher04,"variable.labels")

# RedoNames for Y4 --------------------------------------------------------

label(Teacher04$respid  ) <- "RESPONDENT ID: SITE+DISTRICT+SCHOOL+TEACHER+T"
label(Teacher04$geosteid) <- "GEOSITE ID"
label(Teacher04$dstrctid) <- "DISTRICT ID: GEOSITE+DISTRICT"
label(Teacher04$schoolid) <- "SCHOOL ID: GEOSITE+DISTRICT+SCHOOL"
label(Teacher04$intv    ) <- "PROGRAM"
label(Teacher04$dc_year ) <- "YEAR"
label(Teacher04$y4status) <- "YEAR 4 STATUS"
label(Teacher04$rsltdate) <- "RESULT DATE"
label(Teacher04$tq4_1a  ) <- "Teachers respect colleagues who are expert"
label(Teacher04$tq4_1b  ) <- "Teachers trust each other"
label(Teacher04$tq4_1c  ) <- "Teachers really care about each other"
label(Teacher04$tq4_1d  ) <- "Teachers respect other teachers who lead in school improvement"
label(Teacher04$tq4_1e  ) <- "Teachers openly express their professional views at faculty meetings"
label(Teacher04$tq4_1f  ) <- "Teachers are willing to question one another's views"
label(Teacher04$tq4_1g  ) <- "We do a good job of talking through views, opinions, and values"
label(Teacher04$tq4_1h  ) <- "Teachers are expected to continually learn and seek out new ideas "
label(Teacher04$tq4_1i  ) <- "Teachers are encouraged to experiment in their classrooms "
label(Teacher04$tq4_1j  ) <- "Teachers are encouraged to take risks in order to improve their teaching"
label(Teacher04$tq4_1k  ) <- "Teachers expect students to complete every assignment"
label(Teacher04$tq4_1l  ) <- "Teachers encourage students to keep trying even when the work is challenging"
label(Teacher04$tq4_1m  ) <- "Teachers set high expectations for academic work"
label(Teacher04$tq4_1n  ) <- "Teachers think it's important that all students do well in their classes"
label(Teacher04$tq4_2a  ) <- "Num teachers..Take responsibility for helping one another do well"
label(Teacher04$tq4_2b  ) <- "Num teachers..Help maintain positive student behavior in the entire school"
label(Teacher04$tq4_2c  ) <- "Num teachers..Take responsibility for improving the overall quality of teaching in the school"
label(Teacher04$tq4_3a  ) <- "Policies about how I should teach are often contradictory"
label(Teacher04$tq4_3b  ) <- "difficulty choosing what to do out of all the options"
label(Teacher04$tq4_3c  ) <- "I am often unsure about how to prioritize things"
label(Teacher04$tq4_3d  ) <- "instructional policies seem inconsistent"
label(Teacher04$tq4_4a  ) <- "I know content covered and methods used by other teachers at this school"
label(Teacher04$tq4_4b  ) <- "When I begin with new students, I know what they learned previously"
label(Teacher04$tq4_4c  ) <- "It's easy for other teachers  to know what students learned in my class"
label(Teacher04$tq4_4d  ) <- "I frequently plan and coordinate instruction with my students' other teachers"
label(Teacher04$tq4_4e  ) <- "teachers with similar kids use similar methods and content"
label(Teacher04$tq4_4f  ) <- "Students are expected to master content before moving on"
label(Teacher04$tq4_5a  ) <- "Percentage students.. LEP or ESL"
label(Teacher04$tq4_5b  ) <- "Percentage students.. emotionally or behaviorally disordered"
label(Teacher04$tq4_5c  ) <- "Percentage students.. LD or Mentally Impaired"

label(Teacher04$tq4_6   ) <- "Do you teach read?"
label(Teacher04$tq4_7a  ) <- "teach read to more than one group each day"
label(Teacher04$tq4_7b  ) <- "teach read to several groups with periodic reassignment"
label(Teacher04$tq4_7c  ) <- "Self-Contained"
label(Teacher04$tq4_8   ) <- "read ClassSize"
label(Teacher04$tq4_9   ) <- "How students assigned to you?"
label(Teacher04$tq4_10  ) <- "How often students change?"
label(Teacher04$tq4_11  ) <- "Primary grade level"
label(Teacher04$tq4_12  ) <- "RC of St compared to national average"
label(Teacher04$tq4_13a ) <- "Most in my class can learn what I am supposed to teach them"
label(Teacher04$tq4_13b ) <- "By trying different methods, I can significantly affect achievement"
label(Teacher04$tq4_13c ) <- "I feel satisfaction when students learn what I teach them"
label(Teacher04$tq4_14  ) <- "How many minutes of class"
label(Teacher04$tq4_15a ) <- "[read] How often. Whole class grouping"
label(Teacher04$tq4_15b ) <- "[read] How often. Ability grouping"
label(Teacher04$tq4_15c ) <- "[read] How often. Mixed ability grouping"
label(Teacher04$tq4_15d ) <- "[read] How often. Individualized instruction"
label(Teacher04$tq4_16a ) <- "[Primary focus] Word Analysis"
label(Teacher04$tq4_16b ) <- "[Primary focus] read fluency"
label(Teacher04$tq4_16c ) <- "[Primary focus] Listening Comprehension"
label(Teacher04$tq4_16d ) <- "[Primary focus] read Comprehension"
label(Teacher04$tq4_16e ) <- "[Primary focus] Grammar"
label(Teacher04$tq4_16f ) <- "[Primary focus] Spelling"
label(Teacher04$tq4_16g ) <- "[Primary focus] Written composition"
label(Teacher04$tq4_17a ) <- "[Primary focus] Phonics w words in sentences"
label(Teacher04$tq4_17b ) <- "[Primary focus] Context to read words"
label(Teacher04$tq4_17c ) <- "[Primary focus] Sound blending"
label(Teacher04$tq4_17d ) <- "[Primary focus] Sound segmenting"
label(Teacher04$tq4_17e ) <- "[Primary focus] sight word"
label(Teacher04$tq4_18a ) <- "[Primary focus] prior knowledge or personal"
label(Teacher04$tq4_18b ) <- "[Primary focus] Students generating questions"
label(Teacher04$tq4_18c ) <- "[Primary focus] Summarizing"
label(Teacher04$tq4_18d ) <- "[Primary focus] Analyzing or evaluating text"
label(Teacher04$tq4_18e ) <- "[Primary focus] Examining literary techniques"
label(Teacher04$tq4_18f ) <- "[Primary focus] Identifying the author's purpose"
label(Teacher04$tq4_18g ) <- "[Primary focus] concept maps, story maps, or text structure"
label(Teacher04$tq4_18h ) <- "[Primary focus] Answering explicit questions"
label(Teacher04$tq4_18i ) <- "[Primary focus] Answering implicit questions"
label(Teacher04$tq4_19a ) <- "[demonstrate comprehension] Wrote brief answers"
label(Teacher04$tq4_19b ) <- "[demonstrate comprehension] Wrote extensive answers"
label(Teacher04$tq4_19c ) <- "[demonstrate comprehension] think-aloud or explained application"
label(Teacher04$tq4_19d ) <- "[demonstrate comprehension] written literature extension project"
label(Teacher04$tq4_20a ) <- "[students work on] Editing capitalization..."
label(Teacher04$tq4_20b ) <- "[students work on] Editing word use, grammar, or syntax"
label(Teacher04$tq4_20c ) <- "[students work on] Revising by elaborating"
label(Teacher04$tq4_20d ) <- "[students work on] Revising by reorganizing"
label(Teacher04$tq4_21a ) <- "[write] Using only letter strings or words"
label(Teacher04$tq4_21b ) <- "[write] sentence or separate sentences"
label(Teacher04$tq4_21c ) <- "[write] paragraph or separate paragraphs"
label(Teacher04$tq4_21d ) <- "[write] Two or more connected paragraphs"
label(Teacher04$tq4_22a ) <- "[listening comprehension] Informational text"
label(Teacher04$tq4_22b ) <- "[listening comprehension] Chapter book"
label(Teacher04$tq4_23a ) <- "[read comprehension] Informational text"
label(Teacher04$tq4_23b ) <- "[read comprehension] Narrative with patterning"
label(Teacher04$tq4_23c ) <- "[read comprehension] Narrative with controlled vocabulary"
label(Teacher04$tq4_23d ) <- "[read comprehension] Short uncontrolled narrative"
label(Teacher04$tq4_23e ) <- "[read comprehension] Chapter book"
label(Teacher04$tq4_24a ) <- "[Mrs. Jones should . for read fluency]  repeated reads"
label(Teacher04$tq4_24b ) <- "[Mrs. Jones should . for read fluency]  Teach which parts of a passage to skip"
label(Teacher04$tq4_24c ) <- "[Mrs. Jones should . for read fluency]  Read aloud more frequently to increase interest"
label(Teacher04$tq4_24d ) <- "[Mrs. Jones should . for read fluency]  Provide high-interest books and choice"
label(Teacher04$tq4_25a ) <- "[Is Word high frequency] said"
label(Teacher04$tq4_25b ) <- "[Is Word high frequency] and"
label(Teacher04$tq4_25c ) <- "[Is Word high frequency] bear"
label(Teacher04$tq4_25d ) <- "[Is Word high frequency] was"
label(Teacher04$tq4_25e ) <- "[Is Word irregularly spelled] said"
label(Teacher04$tq4_25f ) <- "[Is Word irregularly spelled] and"
label(Teacher04$tq4_25g ) <- "[Is Word irregularly spelled] bear"
label(Teacher04$tq4_25h ) <- "[Is Word irregularly spelled] was"
label(Teacher04$tq4_26a ) <- "[What meaning of fable?] Birds of a feather flock together."
label(Teacher04$tq4_26b ) <- "[What meaning of fable?] One can not escape one's own evil deeds."
label(Teacher04$tq4_26c ) <- "[What meaning of fable?] The gods help those that help themselves."
label(Teacher04$tq4_26d ) <- "[What meaning of fable?] The hero is brave in deeds as well as words."
label(Teacher04$tq4_24ax) <- "[To fluency, Jones do]  repeated reads"
label(Teacher04$tq4_24bx) <- "[To fluency, Jones do]  Teach which parts of a passage to skip"
label(Teacher04$tq4_24cx) <- "[To fluency, Jones do]  Read aloud more frequently to increase interest"
label(Teacher04$tq4_24dx) <- "[To fluency, Jones do]  Provide high-interest books and choice"
label(Teacher04$tq4_25ax) <- "[Is Word high frequency] said"
label(Teacher04$tq4_25bx) <- "[Is Word high frequency] and"
label(Teacher04$tq4_25cx) <- "[Is Word high frequency] bear"
label(Teacher04$tq4_25dx) <- "[Is Word high frequency] was"
label(Teacher04$tq4_25ex) <- "[Is Word irregularly spelled] said"
label(Teacher04$tq4_25fx) <- "[Is Word irregularly spelled] and"
label(Teacher04$tq4_25gx) <- "[Is Word irregularly spelled] bear"
label(Teacher04$tq4_25hx) <- "[Is Word irregularly spelled] was"
label(Teacher04$tq4_26ax) <- "[What meaning of fable?]  Birds of a feather flock together."
label(Teacher04$tq4_26bx) <- "[What meaning of fable?]  One can not escape one's own evil deeds."
label(Teacher04$tq4_26cx) <- "[What meaning of fable?]  The gods help those that help themselves."
label(Teacher04$tq4_26dx) <- "[What meaning of fable?]  The hero is brave in deeds as well as words."

label(Teacher04$tq4_27  ) <- "teach math?"
label(Teacher04$tq4_28a ) <- "teach math to more than one class of students each day"
label(Teacher04$tq4_28b ) <- "teach math to several groups with periodic reassignment"
label(Teacher04$tq4_28c ) <- "teach math to only one class of students this year"
label(Teacher04$tq4_29  ) <- "Math ClassSize?"
label(Teacher04$tq4_30  ) <- "How students assigned to you?"
label(Teacher04$tq4_31  ) <- "How often students change?"
label(Teacher04$tq4_32  ) <- "What grade level"
label(Teacher04$tq4_33  ) <- "math performance of St compared to national average"
label(Teacher04$tq4_34a ) <- "Most in class can learn"
label(Teacher04$tq4_34b ) <- "By trying different methods, I can significantly affect achievement"
label(Teacher04$tq4_34c ) <- "I feel a great deal of satisfaction when students learn"
label(Teacher04$tq4_35  ) <- "how many minutes math class?"
label(Teacher04$tq4_36a ) <- "[Math] How often Whole class grouping"
label(Teacher04$tq4_36b ) <- "[Math] How often Ability grouping"
label(Teacher04$tq4_36c ) <- "[Math] How often Mixed ability grouping"
label(Teacher04$tq4_36d ) <- "[Math] How often Individualized instruction"
label(Teacher04$tq4_37a ) <- "[frequent focus] Only whole numbers 0-20"
label(Teacher04$tq4_37b ) <- "[frequent focus] Whole numbers 0-100"
label(Teacher04$tq4_37c ) <- "[frequent focus] Whole numbers > 100"
label(Teacher04$tq4_37d ) <- "[frequent focus] Negative numbers"
label(Teacher04$tq4_37e ) <- "[frequent focus] Fractions"
label(Teacher04$tq4_37f ) <- "[frequent focus] Decimals"
label(Teacher04$tq4_38a ) <- "[focus on] Counting"
label(Teacher04$tq4_38b ) <- "[focus on] Number concepts with whole numbers"
label(Teacher04$tq4_38c ) <- "[focus on] Number concepts with fractions and decimals"
label(Teacher04$tq4_38d ) <- "[focus on] Addition"
label(Teacher04$tq4_38e ) <- "[focus on] Subtraction"
label(Teacher04$tq4_38f ) <- "[focus on] Multiplication"
label(Teacher04$tq4_38g ) <- "[focus on] Division"
label(Teacher04$tq4_38h ) <- "[focus on] Creating, continuing, or explaining patterns or sequences"
label(Teacher04$tq4_38i ) <- "[focus on] Functions or algebra"
label(Teacher04$tq4_38j ) <- "[focus on] Geometry or spatial sense"
label(Teacher04$tq4_38k ) <- "[focus on] Measurement"
label(Teacher04$tq4_38l ) <- "[focus on] Creating or using tables, tallies, graphs or charts"
label(Teacher04$tq4_39a ) <- "[topics in operations] The meaning or properties of an operation"
label(Teacher04$tq4_39b ) <- "[topics in operations] Methods or strategies for finding answers"
label(Teacher04$tq4_39c ) <- "[topics in operations] Practicing basic facts for speed or accuracy"
label(Teacher04$tq4_39d ) <- "[topics in operations] Why a conventional computational procedure works"
label(Teacher04$tq4_39e ) <- "[topics in operations] steps of a conventional computation procedure"
label(Teacher04$tq4_39f ) <- "[topics in operations] Practicing procedures for speed, accuracy, or ease"
label(Teacher04$tq4_39g ) <- "[topics in operations] transitional, alternative, or non-conventional methods"
label(Teacher04$tq4_39h ) <- "[topics in operations] Applying basic facts to word problems"
label(Teacher04$tq4_39i ) <- "[topics in operations] Estimating the answer"
label(Teacher04$tq4_40a ) <- "[Frequency] Listen to definition or the steps of a procedure"
label(Teacher04$tq4_40b ) <- "[Frequency] Perform tasks requiring already introduced"
label(Teacher04$tq4_40c ) <- "[Frequency] Assess a problem and choose a method"
label(Teacher04$tq4_40d ) <- "[Frequency] tasks requiring methods not introduced"
label(Teacher04$tq4_40e ) <- "[Frequency] Explain an answer or a solution method"
label(Teacher04$tq4_40f ) <- "[Frequency] simil and diff among representations, solutions, or methods"
label(Teacher04$tq4_40g ) <- "[Frequency] Prove that a method works for all similar cases"
label(Teacher04$tq4_41a ) <- "[Frequency] problems with multiple answers or solution methods"
label(Teacher04$tq4_41b ) <- "[Frequency] Discuss ideas, problems, solutions, or methods"
label(Teacher04$tq4_41c ) <- "[Frequency] Write extended explanations of ideas, solutions, or methods"
label(Teacher04$tq4_41d ) <- "[Frequency] Work on a investigation, problem, or project for several days"
label(Teacher04$tq4_42  ) <- "name and/or publisher of the math materials"
label(Teacher04$tq4_43a ) <- "[ways to represent 391]  2 hundreds + 19 tens + 1 one"
label(Teacher04$tq4_43b ) <- "[ways to represent 391]  3 hundreds + 9 tens + 10 tenths"
label(Teacher04$tq4_43c ) <- "[ways to represent 391]  39 tens + 1 one"
label(Teacher04$tq4_44  ) <- "Write 72 in Base6"
label(Teacher04$tq4_45  ) <- "What power 10 equals 1"
label(Teacher04$tq4_46a ) <- "M has 3 pizzas and Gives Half to Friend"
label(Teacher04$tq4_46b ) <- "D has 3c Choc Chips. How many batches of can he make if each takes .5 cups"
label(Teacher04$tq4_46c ) <- "3 have .5 cookie. How many total"
label(Teacher04$tq4_43ax) <- "[ways to represent 391]  2 hundreds + 19 tens + 1 one"
label(Teacher04$tq4_43bx) <- "[ways to represent 391]  3 hundreds + 9 tens + 10 tenths"
label(Teacher04$tq4_43cx) <- "[ways to represent 391]  39 tens + 1 one"
label(Teacher04$tq4_44x ) <- "Write 72 in Base6"
label(Teacher04$tq4_45x ) <- "What power 10 equals 1"
label(Teacher04$tq4_46ax) <- "M has 3 pizzas and Gives Half to Friend"
label(Teacher04$tq4_46bx) <- "D has 3c Choc Chips. How many batches of can he make if each takes .5 cups"
label(Teacher04$tq4_46cx) <- "3 have .5 cookie. How many total"
label(Teacher04$tq4_47a ) <- "[reform models] Accelerated Schools Project"
label(Teacher04$tq4_47b ) <- "[reform models] America's Choice"
label(Teacher04$tq4_47c ) <- "[reform models] Roots and Wings"
label(Teacher04$tq4_47d ) <- "[reform models] Success for All"
label(Teacher04$tq4_47e ) <- "[reform models] ...a program that is not listed here"
label(Teacher04$tq4_48a ) <- "detailed plan for improving instruction"
label(Teacher04$tq4_48b ) <- "The steps for improving instruction are carefully staged and sequenced"
label(Teacher04$tq4_48c ) <- "Steps that teachers should take to promote classroom improvement are clearly outlined"
label(Teacher04$tq4_48d ) <- "Instructional goals for students are clearly defined"
label(Teacher04$tq4_48e ) <- "participation exposed me to examples of student work sought"
label(Teacher04$tq4_48f ) <- "participation exposed me to examples of classroom teaching sought"
label(Teacher04$tq4_48g ) <- "CSR staff provided useful ideas and resources"
label(Teacher04$tq4_49a) <- "I am capable of changes called for by CSR"
label(Teacher04$tq4_49b) <- "Changes for CSR help students achieve"
label(Teacher04$tq4_49c) <- "CSR requires major changes"
label(Teacher04$tq4_49d) <- "value changes called for by CSR"
label(Teacher04$tq4_50 ) <- "Hours PD"
label(Teacher04$tq4_51a) <- "[PD foucs] Student assessment"
label(Teacher04$tq4_51b) <- "[PD foucs] Curriculum materials or frameworks"
label(Teacher04$tq4_51c) <- "[PD foucs] Content or performance standards"
label(Teacher04$tq4_51d) <- "[PD foucs] Teaching methods"
label(Teacher04$tq4_51f) <- "[PD foucs] Multicultural or diversity issues"
label(Teacher04$tq4_51g) <- "[PD foucs] Classroom management / discipline"
label(Teacher04$tq4_51h) <- "[PD foucs] School governance"
label(Teacher04$tq4_51i) <- "[PD foucs] School improvement planning"
label(Teacher04$tq4_51j) <- "[PD foucs] Social services for students"
label(Teacher04$tq4_51k) <- "[PD foucs] Safety or school climate issues"
label(Teacher04$tq4_51l) <- "[PD foucs] Parent involvement"
label(Teacher04$tq4_52a) <- "[effort on] Math curriculum materials"
label(Teacher04$tq4_52b) <- "[effort on] skills at designing math tasks"
label(Teacher04$tq4_52c) <- "[effort on] knowledge of number concepts"
label(Teacher04$tq4_52d) <- "[effort on] knowledge of particular computational procedures"
label(Teacher04$tq4_52f) <- "[effort on] representations for number concepts"
label(Teacher04$tq4_52g) <- "[effort on] representations for operations or computation"
label(Teacher04$tq4_53a) <- "[effort on] ELA curriculum materials"
label(Teacher04$tq4_53b) <- "[effort on] skills at doing miscue analysis"
label(Teacher04$tq4_53c) <- "[effort on] skills at designing ELA tasks for my students"
label(Teacher04$tq4_53d) <- "[effort on] knowledge of phonetics"
label(Teacher04$tq4_53e) <- "[effort on] knowledge of guided read strategies-context clues"
label(Teacher04$tq4_53f) <- "[effort on] knowledge of the writing process"
label(Teacher04$tq4_53g) <- "[effort on] help students blend and segment sounds"
label(Teacher04$tq4_53h) <- "[effort on] comprehension strategies eg KWL"
label(Teacher04$tq4_54a) <- "[work with other] Clarifying standards through analysis students work"
label(Teacher04$tq4_54b) <- "[work with other] Developing thematic units"
label(Teacher04$tq4_54c) <- "[work with other] scope or sequence"
label(Teacher04$tq4_54d) <- "[work with other] alignment of materials and assessments"
label(Teacher04$tq4_54e) <- "[work with other] use particular grouping strategies"
label(Teacher04$tq4_55a) <- "[How often] watched teacher model"
label(Teacher04$tq4_55b) <- "[How often] teacher observed me w feedback"
label(Teacher04$tq4_55c) <- "[How often] gave another feedback"
label(Teacher04$tq4_56a) <- "[How often] watched an leader"
label(Teacher04$tq4_56b) <- "[How often] leader observed w feedback about techniques"
label(Teacher04$tq4_56c) <- "[How often] leader observed w feedback about materials"
label(Teacher04$tq4_56d) <- "[How often] leader studied my students' work"
label(Teacher04$tq4_57a) <- "[PD] Gave opportunities to focus on what I am trying to develop"
label(Teacher04$tq4_57b) <- "[PD] Provided me with useful knowledge"
label(Teacher04$tq4_57c) <- "[PD] coherently related"
label(Teacher04$tq4_57d) <- "[PD] focus on a problem over an extended period of time"
label(Teacher04$tq4_57e) <- "[PD] Focused on too many topics"
label(Teacher04$tq4_57f) <- "[PD] Provided me with useful feedback about my teaching"
label(Teacher04$tq4_57g) <- "[PD] Made me pay closer attention to particular things I was doing"
label(Teacher04$tq4_57h) <- "[PD] Led me to seek out additional information"
label(Teacher04$tq4_57i) <- "[PD] Led me to think about an aspect of my teaching in a new way"
label(Teacher04$tq4_57j) <- "[PD] Led me to try new things in the classroom"
label(Teacher04$tq4_58 ) <- "SEX"
label(Teacher04$tq4_59 ) <- "RACE"
label(Teacher04$tq4_60 ) <- "Employment status"
label(Teacher04$tq4_61a) <- "MAIN teaching assignment?"
label(Teacher04$tq4_61b) <- "[Specialist] MAIN teaching assignment"
label(Teacher04$tq4_62 ) <- "Years Exp"
label(Teacher04$tq4_63 ) <- "Years Exp at school"
label(Teacher04$tq4_64 ) <- "undergraduate major"
label(Teacher04$tq4_65 ) <- "major for highest graduate degree?"
label(Teacher04$tq4_66a) <- "[certification] PERMANENT OR STANDARD CERTIFICATION"
label(Teacher04$tq4_66b) <- "[certification] PROBATIONARY CERTIFICATION"
label(Teacher04$tq4_66c) <- "[certification] TEMPORARY CERTIFICATION"
label(Teacher04$tq4_66d) <- "[certification] ALTERNATIVE CERTIFICATION"
label(Teacher04$tq4_66e) <- "[certification] NOT CERTIFIED"
label(Teacher04$tq4_67a) <- "[UG or G classes] English or a related ELA field"
label(Teacher04$tq4_67b) <- "[UG or G classes] Methods of teaching read, English, and/or ELA"
label(Teacher04$tq4_67c) <- "[UG or G classes] math"
label(Teacher04$tq4_67d) <- "[UG or G classes] Methods of teaching math"
label(Teacher04$tq4_68a) <- "[Non-university PD hours] ELA"
label(Teacher04$tq4_68b) <- "[Non-university PD hours] math"
label(Teacher04)

Teacher04 <- rename(Teacher04, c(intv     ="PROGRAM",
                                 dc_year  ="YEAR",
                                 tq4_1a   ="T.RespExpert",
                                 tq4_1b   ="T.Trust",
                                 tq4_1c   ="T.Care",
                                 tq4_1d   ="T.RespectCSRLead",
                                 tq4_1e   ="T.OpenExpress",
                                 tq4_1f   ="T.QuestionEachother",
                                 tq4_1g   ="T.TalkThroughViews",
                                 tq4_1h   ="T.ExpectLearn",
                                 tq4_1i   ="T.EncourageExperiment",
                                 tq4_1j   ="T.EncourageRisk",
                                 tq4_1k   ="T.ExpectStWork",
                                 tq4_1l   ="T.EncourageSt",
                                 tq4_1m   ="T.HighExpectation",
                                 tq4_1n   ="T.AllDoWell",
                                 tq4_2a   ="NT.HelpOthers",
                                 tq4_2b   ="NT.StBehavior",
                                 tq4_2c   ="NT.OverallQuality",
                                 tq4_3a   ="Guide.PolicyConfusion",
                                 tq4_3b   ="Guide.ProblemChoose",
                                 tq4_3c   ="Guide.UnsurePriority",
                                 tq4_3d   ="Guide.PolicyConsistency",
                                 tq4_4a   ="Links.Content",
                                 tq4_4b   ="Links.KnowWhatStKnow",
                                 tq4_4c   ="Links.StLearnedMyClass",
                                 tq4_4d   ="Links.Coordinate",
                                 tq4_4e   ="Links.SimilarMethods",
                                 tq4_4f   ="Links.MasterContent",
                                 tq4_5a   ="PercLEP",
                                 tq4_5b   ="PercEBD",
                                 tq4_5c   ="PercLD",
                                 tq4_6    ="TeachRead",
                                 tq4_7a   ="R.Struct.MultGroup",
                                 tq4_7b   ="R.Struct.Reassign",
                                 tq4_7c   ="R.Struct.Contained",
                                 tq4_8    ="R.ClassSize",
                                 tq4_9    ="R.St.Assigned",
                                 tq4_10   ="R.StChange",
                                 tq4_11   ="R.Grade",
                                 tq4_12   ="R.Achievement",
                                 tq4_13a  ="R.Expectancy",
                                 tq4_13b  ="R.Efficacy",
                                 tq4_13c  ="R.Value",
                                 tq4_14   ="R.Minutes",
                                 tq4_15a  ="R.Group.Whole",
                                 tq4_15b  ="R.Group.Ability",
                                 tq4_15c  ="R.Group.Mixed",
                                 tq4_15d  ="R.Group.Individual",
                                 tq4_16a  ="R.Domain.Analysis",
                                 tq4_16b  ="R.Domain.Fluency",
                                 tq4_16c  ="R.Domain.Listening",
                                 tq4_16d  ="R.Domain.Comprehension",
                                 tq4_16e  ="R.Domain.Grammar",
                                 tq4_16f  ="R.Domain.Spelling",
                                 tq4_16g  ="R.Domain.Composition",
                                 tq4_17a  ="R.Focus.PhonicsSentences",
                                 tq4_17b  ="R.Focus.Context",
                                 tq4_17c  ="R.Focus.Blending",
                                 tq4_17d  ="R.Focus.Segmenting",
                                 tq4_17e  ="R.Focus.Sight",
                                 tq4_18a  ="R.Focus.RC.PriorKnowledge",
                                 tq4_18b  ="R.Focus.RC.GeneratingQuestions",
                                 tq4_18c  ="R.Focus.RC.Summarizing",
                                 tq4_18d  ="R.Focus.RC.Analyzing",
                                 tq4_18e  ="R.Focus.RC.LiteraryTechniques",
                                 tq4_18f  ="R.Focus.RC.AuthorPurpose",
                                 tq4_18g  ="R.Focus.RC.ConceptMaps",
                                 tq4_18h  ="R.Focus.RC.ExplicitQuestions",
                                 tq4_18i  ="R.Focus.RC.ImplicitQuestions",
                                 tq4_19a  ="R.TestRC.WriteBrief",
                                 tq4_19b  ="R.TestRC.WriteExtended",
                                 tq4_19c  ="R.TestRC.ThinkAloud",
                                 tq4_19d  ="R.TestRC.Project",
                                 tq4_20a  ="R.Focus.WR.Proofing",
                                 tq4_20b  ="R.Focus.WR.WordUse",
                                 tq4_20c  ="R.Focus.WR.Elaborating",
                                 tq4_20d  ="R.Focus.WR.Reorganizing",
                                 tq4_21a  ="R.Write.Words",
                                 tq4_21b  ="R.Write.Sentence",
                                 tq4_21c  ="R.Write.Paragraph",
                                 tq4_21d  ="R.Write.Connected",
                                 tq4_22a  ="R.Text.LC.Informational",
                                 tq4_22b  ="R.Text.LC.Narrative",
                                 tq4_23a  ="R.Text.RC.Informational",
                                 tq4_23b  ="R.Text.RC.NarrativePatterning",
                                 tq4_23c  ="R.Text.RC.NarrativeControlled",
                                 tq4_23d  ="R.Text.RC.ShortBook",
                                 tq4_23e  ="R.Text.RC.Novel",
                                 tq4_24a  ="R.TKnowl.Fluency.Repeated"    , tq4_24ax ="R.GTKnowl.Fluency.Repeated"    ,
                                 tq4_24b  ="R.TKnowl.Fluency.Skip"        , tq4_24bx ="R.GTKnowl.Fluency.Skip"        ,   
                                 tq4_24c  ="R.TKnowl.Fluency.ReadAloud"   , tq4_24cx ="R.GTKnowl.Fluency.ReadAloud"   ,
                                 tq4_24d  ="R.TKnowl.Fluency.HighInterest", tq4_24dx ="R.GTKnowl.Fluency.HighInterest",
                                 tq4_25a  ="R.TKnowl.Freq.Said"           , tq4_25ax ="R.GTKnowl.Freq.Said"           ,
                                 tq4_25b  ="R.TKnowl.Freq.And"            , tq4_25bx ="R.GTKnowl.Freq.And"            ,
                                 tq4_25c  ="R.TKnowl.Freq.Bear"           , tq4_25cx ="R.GTKnowl.Freq.Bear"           ,
                                 tq4_25d  ="R.TKnowl.Freq.Was"            , tq4_25dx ="R.GTKnowl.Freq.Was"            ,
                                 tq4_25e  ="R.TKnowl.Irr.Said"            , tq4_25ex ="R.GTKnowl.Irr.Said"            ,
                                 tq4_25f  ="R.TKnowl.Irr.And"             , tq4_25fx ="R.GTKnowl.Irr.And"             ,
                                 tq4_25g  ="R.TKnowl.Irr.Bear"            , tq4_25gx ="R.GTKnowl.Irr.Bear"            ,
                                 tq4_25h  ="R.TKnowl.Irr.Was"             , tq4_25hx ="R.GTKnowl.Irr.Was"             ,
                                 tq4_26a  ="R.TKnowl.Fable.Birds"         , tq4_26ax ="R.GTKnowl.Fable.Birds"         ,
                                 tq4_26b  ="R.TKnowl.Fable.Evil"          , tq4_26bx ="R.GTKnowl.Fable.Evil"          ,
                                 tq4_26c  ="R.TKnowl.Fable.HelpSelf"      , tq4_26cx ="R.GTKnowl.Fable.HelpSelf"      ,
                                 tq4_26d  ="R.TKnowl.Fable.Deeds"         , tq4_26dx ="R.GTKnowl.Fable.Deeds"         ,
                                 
                                 tq4_27   ="TeachMath",
                                 tq4_28a  ="M.Struct.MultGroup",
                                 tq4_28b  ="M.Struct.Reassign",
                                 tq4_28c  ="M.Struct.Contained",
                                 tq4_29   ="M.ClassSize",
                                 tq4_30   ="M.St.Assigned",
                                 tq4_31   ="M.StChange",
                                 tq4_32   ="M.Grade",
                                 tq4_33   ="M.Achievement",
                                 tq4_34a  ="M.Expectancy",
                                 tq4_34b  ="M.Efficacy",
                                 tq4_34c  ="M.Value",
                                 tq4_35   ="M.Minutes",
                                 tq4_36a  ="M.Group.Whole",
                                 tq4_36b  ="M.Group.Ability",
                                 tq4_36c  ="M.Group.Mixed",
                                 tq4_36d  ="M.Group.Individual",
                                 tq4_37a  ="M.Content.Less20",
                                 tq4_37b  ="M.Content.0to100",
                                 tq4_37c  ="M.Content.Greater100",
                                 tq4_37d  ="M.Content.Negative",
                                 tq4_37e  ="M.Content.Fractions",
                                 tq4_37f  ="M.Content.Decimals",
                                 tq4_38a  ="M.Domain.Counting",
                                 tq4_38b  ="M.Domain.Integer",
                                 tq4_38c  ="M.Domain.Fractions",
                                 tq4_38d  ="M.Domain.Addition",
                                 tq4_38e  ="M.Domain.Subtraction",
                                 tq4_38f  ="M.Domain.Multiplication",
                                 tq4_38g  ="M.Domain.Division",
                                 tq4_38h  ="M.Domain.Patterns",
                                 tq4_38i  ="M.Domain.Functions",
                                 tq4_38j  ="M.Domain.Geometry",
                                 tq4_38k  ="M.Domain.Measurement",
                                 tq4_38l  ="M.Domain.Graphs",
                                 tq4_39a  ="M.Focus.Op.Properties",
                                 tq4_39b  ="M.Focus.Op.Strategies",
                                 tq4_39c  ="M.Focus.Op.PracticeFacts",
                                 tq4_39d  ="M.Focus.Op.WhyProcedure",
                                 tq4_39e  ="M.Focus.Op.Procedure",
                                 tq4_39f  ="M.Focus.Op.PracticingProcedures",
                                 tq4_39g  ="M.Focus.Op.NonCoventionalMethods",
                                 tq4_39h  ="M.Focus.Op.WordProblems",
                                 tq4_39i  ="M.Focus.Op.Estimating",
                                 tq4_40a  ="M.Focus.Lecture",
                                 tq4_40b  ="M.Focus.Practice",
                                 tq4_40c  ="M.Focus.ChooseMethod",
                                 tq4_40d  ="M.Focus.DiscoverProcedure",
                                 tq4_40e  ="M.Focus.Explain",
                                 tq4_40f  ="M.Focus.Compare",
                                 tq4_40g  ="M.Focus.Proof",
                                 tq4_41a  ="M.Focus.MultAns",
                                 tq4_41b  ="M.Focus.Discuss",
                                 tq4_41c  ="M.Focus.Write",
                                 tq4_41d  ="M.Focus.Project",
                                 tq4_42   ="Materials",
                                 tq4_43a  ="M.TKnowl.391a"  , tq4_43ax  ="M.GTKnowl.391a"  ,
                                 tq4_43b  ="M.TKnowl.391b"  , tq4_43bx  ="M.GTKnowl.391b"  , 
                                 tq4_43c  ="M.TKnowl.391c"  , tq4_43cx  ="M.GTKnowl.391c"  , 
                                 tq4_44   ="M.TKnowl.72B6"  , tq4_44x   ="M.GTKnowl.72B6"  , 
                                 tq4_45   ="M.TKnowl.10Exp1", tq4_45x   ="M.GTKnowl.10Exp1", 
                                 tq4_46a  ="M.TKnowl.3T2a"  , tq4_46ax  ="M.GTKnowl.3T2a"  , 
                                 tq4_46b  ="M.TKnowl.3T2b"  , tq4_46bx  ="M.GTKnowl.3T2b"  , 
                                 tq4_46c  ="M.TKnowl.3T2c"  , tq4_46cx  ="M.GTKnowl.3T2c"  , 
                                 tq4_47a  ="ASP",
                                 tq4_47b  ="AC",
                                 tq4_47c  ="RaW",
                                 tq4_47d  ="SFA",
                                 tq4_47e  ="CSRother",
                                 tq4_48a  ="Specificity.DetailedPlan",
                                 tq4_48b  ="Specificity.StepsSequenced",
                                 tq4_48c  ="Specificity.StepsClear",
                                 tq4_48d  ="Specificity.GoalsClear",
                                 tq4_48e  ="Specificity.ExampleStWork",
                                 tq4_48f  ="Specificity.ExampleTeaching",
                                 tq4_48g  ="Specificity.CSRStaffUseful",
                                 tq4_49a  ="CSR.Efficacy",
                                 tq4_49b  ="CSR.Expectancy",
                                 tq4_49c  ="CSR.RequiresChange",
                                 tq4_49d  ="CSR.Value",
                                 tq4_50   ="PD.Hours",
                                 tq4_51a  ="PD.Focus.Assessment",
                                 tq4_51b  ="PD.Focus.Curriculum",
                                 tq4_51c  ="PD.Focus.Standards",
                                 tq4_51d  ="PD.Focus.Teaching",
                                 tq4_51f  ="PD.Focus.Multicultural",
                                 tq4_51g  ="PD.Focus.Management",
                                 tq4_51h  ="PD.Focus.Governance",
                                 tq4_51i  ="PD.Focus.SchoolImprovement",
                                 tq4_51j  ="PD.Focus.SocialServices",
                                 tq4_51k  ="PD.Focus.Safety",
                                 tq4_51l  ="PD.Focus.Parent",
                                 tq4_52a  ="PD.M.Curriculum",
                                 tq4_52b  ="PD.M.DesigningTasks",
                                 tq4_52c  ="PD.M.KnowledgeNumbers",
                                 tq4_52d  ="PD.M.KnowledgeProcedures",
                                 tq4_52f  ="PD.M.RepresentationNumber",
                                 tq4_52g  ="PD.M.RepresentationOperation",
                                 tq4_53a  ="PD.R.Curriculum",
                                 tq4_53b  ="PD.R.Miscue",
                                 tq4_53c  ="PD.R.DesigningTasks",
                                 tq4_53d  ="PD.R.KnowledgePhonetics",
                                 tq4_53e  ="PD.R.KnowledgeGuideRead",
                                 tq4_53f  ="PD.R.KnowledgeWriting",
                                 tq4_53g  ="PD.R.KnowledgeBlending",
                                 tq4_53h  ="PD.R.KnowledgeRCStrategy",
                                 tq4_54a  ="Collab.Standards",
                                 tq4_54b  ="Collab.ThematicUnit",
                                 tq4_54c  ="Collab.ScopeSequence",
                                 tq4_54d  ="Collab.Alignment",
                                 tq4_54e  ="Collab.Grouping",
                                 tq4_55a  ="Supervision.WatchedOther",
                                 tq4_55b  ="Supervision.WatchedMe",
                                 tq4_55c  ="Supervision.FeedbackOther",
                                 tq4_56a  ="Supervision.WatchedLeader",
                                 tq4_56b  ="Supervision.LeaderTechniques",
                                 tq4_56c  ="Supervision.LeaderMaterials",
                                 tq4_56d  ="Supervision.LeaderStWork",
                                 tq4_57a  ="PD.Eval.Relevant",
                                 tq4_57b  ="PD.Eval.UsefulKnowledge",
                                 tq4_57c  ="PD.Eval.Coherent",
                                 tq4_57d  ="PD.Eval.Focused",
                                 tq4_57e  ="PD.Eval.TooBroad",
                                 tq4_57f  ="PD.Eval.UsefulFeedback",
                                 tq4_57g  ="PD.Eval.LedToReflect",
                                 tq4_57h  ="PD.Eval.LedToSeek",
                                 tq4_57i  ="PD.Eval.LedToThink",
                                 tq4_57j  ="PD.Eval.LedToTry",
                                 tq4_58   ="Sex",
                                 tq4_59   ="RACE",
                                 tq4_60   ="Employment",
                                 tq4_61a  ="MainAssignment",
                                 tq4_61b  ="MainAssignmentSpecialist",
                                 tq4_62   ="YearsExp",
                                 tq4_63   ="YearsExpSchool",
                                 tq4_64   ="MajorUG",
                                 tq4_65   ="MajorG",
                                 tq4_66a  ="Cert.Permanent",
                                 tq4_66b  ="Cert.Probationary",
                                 tq4_66c  ="Cert.Temporary",
                                 tq4_66d  ="Cert.Alternative",
                                 tq4_66e  ="Cert.None",
                                 tq4_67a  ="TCourses.ELA",
                                 tq4_67b  ="TCourses.ELAMethods",
                                 tq4_67c  ="TCourses.Math",
                                 tq4_67d  ="TCourses.MathMethods",
                                 tq4_68a  ="PD.Count.ELA",
                                 tq4_68b  ="PD.Count.Math" ))

# # PCK ---------------------------------------------------------------------
# 
# for (i in matchcols(Teacher04,"GTKnowl")) Teacher04[,i] <- unFactor(recode(Teacher04[,i],"'CORRECT'=1; 'NOT CORRECT'=0;"))
# omega(na.omit(Teacher04[,matchcols(Teacher04,"R.GTKnowl")]))
# summary(pca<-prcomp(na.omit(Teacher04[,matchcols(Teacher04,"R.GTKnowl")])))
# biplot(pca)
# Teacher04$R.PCK <- apply(Teacher04[,matchcols(Teacher04,"R.GTKnowl")],1,mean,na.rm=T)
# 
# omega(na.omit(Teacher04[,matchcols(Teacher04,"M.GTKnowl")]))
# summary(pca<-prcomp(na.omit(Teacher04[,matchcols(Teacher04,"M.GTKnowl")])))
# biplot(pca)
# Teacher04$M.PCK <- apply(Teacher04[,matchcols(Teacher04,"M.GTKnowl")],1,mean,na.rm=T)
# 

# SAve Data Set Teacher0-----------------------------------------------------------

for (i in c("R.Value","R.Efficacy","R.Expectancy",
            "M.Value","M.Efficacy","M.Expectancy"))
  Teacher04[,i] <- unFactor(recode(Teacher04[,i],"'STRONGLY AGREE'   =7; 
                                                  'STRONGLY DISAGREE'=1;"))
for (i in c("CSR.Value","CSR.Efficacy","CSR.Expectancy"))
  Teacher04[,i] <- unFactor(recode(Teacher04[,i],"'STRONGLY DISAGREE'=1; 
                                                  'DISAGREE'         =2;
                                                  'AGREE'            =3;
                                                  'STRONGLY AGREE'   =4;"))
for (i in c("M.Achievement","R.Achievement"))
  Teacher04[,i] <- unFactor(recode(Teacher04[,i],"'WELL BELOW NATIONAL AVERAGE'    =-2; 
                                                  'SLIGHTLY BELOW NATIONAL AVERAGE'=-1; 
                                                  'CLOSE TO NATIONAL AVERAGE'      = 0; 
                                                  'SLIGHTLY ABOVE NATIONAL AVERAGE'= 1; 
                                                  'WELL ABOVE NATIONAL AVERAGE'    = 2;"))
for (i in c("R.Struct.MultGroup","R.Struct.Reassign","R.Struct.Contained",
            "M.Struct.MultGroup","M.Struct.Reassign","M.Struct.Contained"))
  Teacher04[,i] <- unFactor(recode(Teacher04[,i],"'YES'=1; 
                                                  'NO' =0;"))

for (i in c("R.Group.Whole","R.Group.Mixed","R.Group.Ability","R.Group.Individual",
            "M.Group.Whole","M.Group.Mixed","M.Group.Ability","M.Group.Individual"))
  Teacher04[,i] <- unFactor(recode(Teacher04[,i],"'RARELY OR NEVER'    =0; 
                                                  'A FEW TIMES A MONTH'=1; 
                                                  'A FEW TIMES A WEEK' =2;
                                                  'EVERY DAY'          =3;"))

for (i in c(matchcols(Teacher04,"R.Focus.RC"),matchcols(Teacher04,"R.TestRC."),matchcols(Teacher04,"R.Write"),matchcols(Teacher04,"R.Focus.WR.")) )
  Teacher04[,i] <- unFactor(recode(Teacher04[,i],"'NEVER'                 =0; 
                                                  'LESS THAN ONCE A MONTH'=1; 
                                                  '1-3 TIMES PER MONTH'   =2;
                                                  '1-2 TIMES PER WEEK'    =3;
                                                  '3-4 TIMES PER WEEK'    =4;
                                                  'EVERY DAY'             =5;"))


Teacher04[,"MajorUG"]<-recode(Teacher04[,"MajorUG"],"'DO NOT HAVE AN UNDERGRADUATE DEGREE'='None'; 'SOCIAL OR BEHAVIORAL SCIENCES'='Soc Sci'; 
'NATURAL/PHYSICAL SCIENCES'='Nat Sci'; 'FOREIGN LANGUAGE'='Lang'")
Teacher04[,"MajorG"]<-recode(Teacher04[,"MajorG"],"'DO NOT HAVE AN UNDERGRADUATE DEGREE'='None'; 'SOCIAL OR BEHAVIORAL SCIENCES'='Soc Sci'; 
'NATURAL/PHYSICAL SCIENCES'='Nat Sci'; 'FOREIGN LANGUAGE'='Lang'")
save(Teacher04,file="data/TeacherSurveyY4.rData")

#LookatMainvariables--------------------------------------------------

load(file="data/TeacherSurveyY4.rData")

describe(Teacher04[,c("R.Value"  ,"R.Efficacy"  ,"R.Expectancy","M.Value"  ,"M.Efficacy"  ,"M.Expectancy","CSR.Value","CSR.Efficacy","CSR.Expectancy")])
summary(sapply(Teacher04[,c("R.Value"  ,"R.Efficacy"  ,"R.Expectancy","M.Value"  ,"M.Efficacy"  ,"M.Expectancy","CSR.Value","CSR.Efficacy","CSR.Expectancy")],factor),10)

xtabs(~TeachRead+TeachMath,Teacher04)
Teacher04$R.FM  <- apply(Teacher04[,c("R.Value"  ,"R.Efficacy"  ,"R.Expectancy"  )],1,mean,na.rm=T)
Teacher04$M.FM  <- apply(Teacher04[,c("M.Value"  ,"M.Efficacy"  ,"M.Expectancy"  )],1,mean,na.rm=T)
Teacher04$CSR.FM<- apply(Teacher04[,c("CSR.Value","CSR.Efficacy","CSR.Expectancy")],1,mean,na.rm=T)
Teacher04$FM<- apply(Teacher04[,c("R.Value"  ,"R.Efficacy"  ,"R.Expectancy","M.Value"  ,"M.Efficacy"  ,"M.Expectancy")],1,mean,na.rm=T)

biplot(prcomp(na.omit(Teacher04[,c("R.Value","R.Efficacy","R.Expectancy","M.Value"  ,"M.Efficacy"  ,"M.Expectancy"  )])))


# Basic Tables ------------------------------------------------------------

barplot(prop.table(xtabs(~CSR.Value     ,Teacher04)),main="Program Valuing")
barplot(prop.table(xtabs(~CSR.Efficacy  ,Teacher04)),main="Program Efficacy")
barplot(prop.table(xtabs(~CSR.Expectancy,Teacher04)),main="Program Expectancy")
mosaicplot(with(Teacher04,xtabs(~CSR.Value+CSR.Efficacy)))
xtabs(~CSR.Value     +CSR.Efficacy,Teacher04)
xtabs(~CSR.Expectancy+CSR.Value   ,Teacher04)
xtabs(~CSR.Expectancy+CSR.Efficacy,Teacher04)

barplot(prop.table(xtabs(~R.Value     ,Teacher04)),main="Reading Valuing")
barplot(prop.table(xtabs(~R.Efficacy  ,Teacher04)),main="Reading Efficacy")
barplot(prop.table(xtabs(~R.Expectancy,Teacher04)),main="Reading Expectancy")
xtabs(~R.Value     +R.Efficacy,Teacher04)
xtabs(~R.Expectancy+R.Value   ,Teacher04)
xtabs(~R.Expectancy+R.Efficacy,Teacher04)

barplot(prop.table(xtabs(~M.Value     ,Teacher04)),main="Math Valuing")
barplot(prop.table(xtabs(~M.Efficacy  ,Teacher04)),main="Math Efficacy")
barplot(prop.table(xtabs(~M.Expectancy,Teacher04)),main="Math Expectancy")
xtabs(~M.Value     +M.Efficacy,Teacher04)
xtabs(~M.Expectancy+M.Value   ,Teacher04)
xtabs(~M.Expectancy+M.Efficacy,Teacher04)


# Reliabilities (Alpha and Omega) -----------------------------------------------

psych::alpha(Teacher04[,c("CSR.Value","CSR.Efficacy","CSR.Expectancy")])
psych::alpha(Teacher04[,c("R.Value"  ,"R.Efficacy"  ,"R.Expectancy")])
psych::alpha(Teacher04[,c("M.Value"  ,"M.Efficacy"  ,"M.Expectancy")])
omega(Teacher04[,c("CSR.Value","CSR.Efficacy","CSR.Expectancy",
                   "R.Value"  ,"R.Efficacy"  ,"R.Expectancy",
                   "M.Value"  ,"M.Efficacy"  ,"M.Expectancy")])
# biplot(prcomp(na.omit((Teacher04[,c("CSR.Value","CSR.Efficacy","CSR.Expectancy",
#                    "R.Value"  ,"R.Efficacy"  ,"R.Expectancy",
#                    "M.Value"  ,"M.Efficacy"  ,"M.Expectancy")]))))


# Correlations ------------------------------------------------------------


cor(Teacher04[,c("CSR.FM","R.FM","M.FM")],use="pair")
cor(Teacher04[,c("CSR.Value","CSR.Efficacy","CSR.Expectancy",
                 "R.Value"  ,"R.Efficacy"  ,"R.Expectancy",
                 "M.Value"  ,"M.Efficacy"  ,"M.Expectancy")],use="pair")
# hetcor(sapply(Teacher04[,c("CSR.Value","CSR.Efficacy","CSR.Expectancy",
#                  "R.Value"  ,"R.Efficacy"  ,"R.Expectancy",
#                  "M.Value"  ,"M.Efficacy"  ,"M.Expectancy")],ordered))$correlations


# SEM and Attenuated Correlations ---------------------------------------------------------------------

# SEM = SD * sqrt(1-reliability)
SEM <- function(sd,r) sd*sqrt(1-r)
SEM(1.02,0.76)
CI <- function(x,mean,r) (mean+r*(x-mean))


Prophecy <- function(rd,r0) rd*(1-r0)/(r0*(1-rd))
AttenCor <- function(cor, r11,r22) cor/sqrt(r11*r22)
#             | CSR  |  M   |  R  |
# Alpha for   | 0.82 | 0.74 | 0.76|

AttenCor(0.22,0.82,0.76) # CSR R = 0.28
AttenCor(0.25,0.82,0.74) # CSR M = 0.32
AttenCor(0.65,0.76,0.74) # R   M = 0.87


# Attenuated Correlations -------------------------------------------------

describe(Teacher04[,c("CSR.FM","R.FM","M.FM")])
cor(Teacher04[,c("CSR.FM","R.FM","M.FM")],use="pair")


# Presage Variables -------------------------------------------------------

boxplot(CSR.FM~R.Grade,Teacher04,main="CSR Force of Motivation by Grade")
boxplot(R.FM  ~R.Grade,Teacher04,main="R Force of Motivation by Grade")
boxplot(M.FM  ~R.Grade,Teacher04,main="M Force of Motivation by Grade")
boxplot(CSR.FM~PROGRAM,Teacher04,main="CSR Force of Motivation by Grade")
boxplot(R.FM  ~PROGRAM,Teacher04,main="R Force of Motivation by Grade")
boxplot(M.FM  ~PROGRAM,Teacher04,main="M Force of Motivation by Grade")
par(mar=c(7,1,1,1))
boxplot(CSR.FM~MajorUG,Teacher04, las=2); 
boxplot(R.FM  ~MajorUG,Teacher04, las=2); 
boxplot(M.FM  ~MajorUG,Teacher04, las=2); 
boxplot(CSR.FM~MajorG,Teacher04, las=2)
boxplot(R.FM  ~MajorG,Teacher04, las=2)
boxplot(M.FM  ~MajorG,Teacher04, las=2)
dev.off()
cor(Teacher04[,c(matchcols(Teacher04,"ClassSize"), matchcols(Teacher04,"Struct"),"R.Achievement","R.Minutes","M.Achievement","M.Minutes",
                 "R.Group.Whole","R.Group.Mixed","R.Group.Ability","R.Group.Individual",
                 "M.Group.Whole","M.Group.Mixed","M.Group.Ability","M.Group.Individual",
                 "YearsExp","YearsExpSchool")],
    Teacher04[,c("CSR.FM","R.FM","M.FM")],use="pair")

summary(lm(CSR.FM~PROGRAM+R.Grade+(R.ClassSize+R.Minutes)+R.Achievement,Teacher04))
summary(lm(CSR.FM~PROGRAM+M.Grade+(M.ClassSize+M.Minutes)+M.Achievement,Teacher04))

summary(lm(R.FM  ~PROGRAM+R.Grade+(R.ClassSize+R.Minutes)+R.Achievement,Teacher04))
summary(lm(M.FM  ~PROGRAM+M.Grade+(M.ClassSize+M.Minutes)+M.Achievement,Teacher04))
# cor(Teacher04[,c(matchcols(Teacher04,"ClassSize"), matchcols(Teacher04,"Struct"),"R.Achievement","R.Minutes","M.Achievement","M.Minutes")],
#     Teacher04[,c("CSR.Value","CSR.Efficacy","CSR.Expectancy","R.Value","M.Value","R.Efficacy","M.Efficacy","R.Expectancy","M.Expectancy")],use="pair")



# Iplots ------------------------------------------------------------------

iplot(jitter(Teacher04$R.FM  ),jitter(Teacher04$M.FM))
iplot(jitter(Teacher04$CSR.FM),jitter(Teacher04$R.FM))
iplot(jitter(Teacher04$CSR.FM),jitter(Teacher04$M.FM))

ihist(Teacher04$R.FM)
ihist(Teacher04$M.FM)
ihist(Teacher04$CSR.FM)

ihist(Teacher04$M.Achievement)
ihist(Teacher04$R.Achievement)
ibar(Teacher04$PROGRAM)
ihist(Teacher04$M.Minutes)
ihist(Teacher04$R.Minutes)

ibar(Teacher04$R.Grade)
ibar(Teacher04$M.Grade)


ibar(Teacher04$R.Efficacy)
ibar(Teacher04$R.Expectancy)
ibar(Teacher04$R.Value)

ibar(Teacher04$M.Efficacy)
ibar(Teacher04$M.Expectancy)
ibar(Teacher04$M.Value)

ibar(Teacher04$CSR.Efficacy)
ibar(Teacher04$CSR.Expectancy)
ibar(Teacher04$CSR.Value)


# Reduce and Export Data Set ----------------------------------------------

data<-Teacher04[,c("respid","dstrctid","schoolid","PROGRAM","YEAR",
                   "y4status","rsltdate","PercLEP","PercEBD","PercLD",
                   "TeachRead",
                   "R.Grade","R.ClassSize","R.Achievement",
                   "R.Expectancy","R.Efficacy","R.Value",
                   "R.Minutes",
                   "R.Group.Whole","R.Group.Ability","R.Group.Mixed","R.Group.Individual",
                   "R.Focus.RC.PriorKnowledge","R.Focus.RC.GeneratingQuestions","R.Focus.RC.Summarizing","R.Focus.RC.Analyzing",
                   "R.Focus.RC.LiteraryTechniques","R.Focus.RC.AuthorPurpose","R.Focus.RC.ConceptMaps","R.Focus.RC.ExplicitQuestions",
                   "R.Focus.RC.ImplicitQuestions",
                   "R.TestRC.WriteBrief","R.TestRC.WriteExtended","R.TestRC.ThinkAloud","R.TestRC.Project",
                   "R.Focus.WR.Proofing","R.Focus.WR.WordUse","R.Focus.WR.Elaborating","R.Focus.WR.Reorganizing",
                   "TeachMath"                       ,
                   "M.Grade","M.ClassSize","M.Achievement",
                   "M.Expectancy","M.Efficacy","M.Value",
                   "M.Minutes",
                   "M.Group.Whole","M.Group.Ability","M.Group.Mixed","M.Group.Individual",
                   "CSR.Efficacy","CSR.Expectancy","CSR.RequiresChange","CSR.Value",
                   "Sex","RACE","YearsExp","YearsExpSchool","MajorUG","MajorG")]
summary(data)
write.csv(data,file="Reduced Teacher Questionnaire Y4.csv")

