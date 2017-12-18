object Main extends App {
//  var day1 = new Day1
//  println(day1.solveA("8231753674683997878179259195565332579493378483264978184143341284379682788518559178822225126625428318115396632681141871952894291898364781898929292614792884883249356728741993224889167928232261325123447569829932951268292953928766755779761837993812528527484487298117739869189415599461746944992651752768158611996715467871381527675219481185217357632445748912726487669881876129192932995282777848496561259839781188719233951619188388532698519298142112853776942545211859134185231768952888462471642851588368445761489225786919778983848113833773768236969923939838755997989537648222217996381757542964844337285428654375499359997792679256881378967852376848812795761118139288152799921176874256377615952758268844139579622754965461884862647423491918913628848748756595463191585555385849335742224855473769411212376446591654846168189278959857681336724221434846946124915271196433144335482787432683848594487648477532498952572515118864475621828118274911298396748213136426357769991314661642612786847135485969889237193822718111269561741563479116832364485724716242176288642371849569664594194674763319687735723517614962575592111286177553435651952853878775431234327919595595658641534765455489561934548474291254387229751472883423413196845162752716925199866591883313638846474321161569892518574346226751366315311145777448781862222126923449311838564685882695889397531413937666673233451216968414288135984394249684886554812761191289485457945866524228415191549168557957633386991931186773843869999284468773866221976873998168818944399661463963658784821796272987155278195355579386768156718813624559264574836134419725187881514665834441359644955768658663278765363789664721736533517774292478192143934318399418188298753351815388561359528533778996296279366394386455544446922653976725113889842749182361253582433319351193862788433113852782596161148992233558144692913791714859516653421917841295749163469751479835492713392861519993791967927773114713888458982796514977717987598165486967786989991998142488631168697963816156374216224386193941566358543266646516247854435356941566492841213424915682394928959116411457967897614457497279472661229548612777155998358618945222326558176486944695689777438164612198225816646583996426313832539918"));
//  println(day1.solveB("8231753674683997878179259195565332579493378483264978184143341284379682788518559178822225126625428318115396632681141871952894291898364781898929292614792884883249356728741993224889167928232261325123447569829932951268292953928766755779761837993812528527484487298117739869189415599461746944992651752768158611996715467871381527675219481185217357632445748912726487669881876129192932995282777848496561259839781188719233951619188388532698519298142112853776942545211859134185231768952888462471642851588368445761489225786919778983848113833773768236969923939838755997989537648222217996381757542964844337285428654375499359997792679256881378967852376848812795761118139288152799921176874256377615952758268844139579622754965461884862647423491918913628848748756595463191585555385849335742224855473769411212376446591654846168189278959857681336724221434846946124915271196433144335482787432683848594487648477532498952572515118864475621828118274911298396748213136426357769991314661642612786847135485969889237193822718111269561741563479116832364485724716242176288642371849569664594194674763319687735723517614962575592111286177553435651952853878775431234327919595595658641534765455489561934548474291254387229751472883423413196845162752716925199866591883313638846474321161569892518574346226751366315311145777448781862222126923449311838564685882695889397531413937666673233451216968414288135984394249684886554812761191289485457945866524228415191549168557957633386991931186773843869999284468773866221976873998168818944399661463963658784821796272987155278195355579386768156718813624559264574836134419725187881514665834441359644955768658663278765363789664721736533517774292478192143934318399418188298753351815388561359528533778996296279366394386455544446922653976725113889842749182361253582433319351193862788433113852782596161148992233558144692913791714859516653421917841295749163469751479835492713392861519993791967927773114713888458982796514977717987598165486967786989991998142488631168697963816156374216224386193941566358543266646516247854435356941566492841213424915682394928959116411457967897614457497279472661229548612777155998358618945222326558176486944695689777438164612198225816646583996426313832539918"));

  var day2 = new Day2
  var day2Input = """
                    |409	194	207	470	178	454	235	333	511	103	474	293	525	372	408	428
                    |4321	2786	6683	3921	265	262	6206	2207	5712	214	6750	2742	777	5297	3764	167
                    |3536	2675	1298	1069	175	145	706	2614	4067	4377	146	134	1930	3850	213	4151
                    |2169	1050	3705	2424	614	3253	222	3287	3340	2637	61	216	2894	247	3905	214
                    |99	797	80	683	789	92	736	318	103	153	749	631	626	367	110	805
                    |2922	1764	178	3420	3246	3456	73	2668	3518	1524	273	2237	228	1826	182	2312
                    |2304	2058	286	2258	1607	2492	2479	164	171	663	62	144	1195	116	2172	1839
                    |114	170	82	50	158	111	165	164	106	70	178	87	182	101	86	168
                    |121	110	51	122	92	146	13	53	34	112	44	160	56	93	82	98
                    |4682	642	397	5208	136	4766	180	1673	1263	4757	4680	141	4430	1098	188	1451
                    |158	712	1382	170	550	913	191	163	459	1197	1488	1337	900	1182	1018	337
                    |4232	236	3835	3847	3881	4180	4204	4030	220	1268	251	4739	246	3798	1885	3244
                    |169	1928	3305	167	194	3080	2164	192	3073	1848	426	2270	3572	3456	217	3269
                    |140	1005	2063	3048	3742	3361	117	93	2695	1529	120	3480	3061	150	3383	190
                    |489	732	57	75	61	797	266	593	324	475	733	737	113	68	267	141
                    |3858	202	1141	3458	2507	239	199	4400	3713	3980	4170	227	3968	1688	4352	4168
                  """.stripMargin;
  println(day2.solveA(day2Input));
  println(day2.solveB(day2Input));

  println(day2.solveB(
    """
      |5 9 2 8
      |9 4 7 3
      |3 8 6 5
    """.stripMargin))
}
