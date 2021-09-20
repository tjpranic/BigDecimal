namespace BigMath

module Tests =

    open NUnit.Framework
    open FsUnit
    open BigMath

    [<Test>]
    let ``Sanity check`` ( ) = 1 |> should equal 1

    // Basic integer arithmetic
    [<Test>][<CategoryAttribute( "Basic Integer Arithmetic" )>]
    let ``1 + 1`` ( ) =
        BigDecimal( 1 ) + BigDecimal( 1 ) |> should equal ( BigDecimal( 2 ) )

    [<Test>][<CategoryAttribute( "Basic Integer Arithmetic" )>]
    let ``2 - 1`` ( ) =
        BigDecimal( 2 ) - BigDecimal( 1 ) |> should equal ( BigDecimal( 1 ) )

    [<Test>][<CategoryAttribute( "Basic Integer Arithmetic" )>]
    let ``2 * 2`` ( ) =
        BigDecimal( 2 ) * BigDecimal( 2 ) |> should equal ( BigDecimal( 4 ) )

    [<Test>][<CategoryAttribute( "Basic Integer Arithmetic" )>]
    let ``9 / 3`` ( ) =
        BigDecimal( 9 ) / BigDecimal( 3 ) |> should equal ( BigDecimal( 3 ) )

    [<Test>][<CategoryAttribute( "Basic Integer Arithmetic" )>]
    let ``3 ** 2`` ( ) =
        BigDecimal( 3 ) ** 2I |> should equal ( BigDecimal( 9 ) )

    [<Test>][<CategoryAttribute( "Basic Integer Arithmetic" )>]
    let ``-7`` ( ) =
        -BigDecimal( 7 ) |> should equal ( BigDecimal( -7 ) )

    // Big integer arithmetic
    [<Test>][<CategoryAttribute( "Big Integer Arithmetic" )>]
    let ``254620731853134930495577620177954948666364525784465071337198373805011 + 100113479515521784939047616965575940940020476738291134367703611737635`` ( ) =
        BigDecimal( 254620731853134930495577620177954948666364525784465071337198373805011I ) + BigDecimal( 100113479515521784939047616965575940940020476738291134367703611737635I ) |> should equal ( BigDecimal( 354734211368656715434625237143530889606385002522756205704901985542646I ) )

    [<Test>][<CategoryAttribute( "Big Integer Arithmetic" )>]
    let ``642694984085765855363758593711106544366621164261442332126564781954604 - 459771059938112487291648013614246326061017391098881287853939368985556`` ( ) =
        BigDecimal( 642694984085765855363758593711106544366621164261442332126564781954604I ) - BigDecimal( 459771059938112487291648013614246326061017391098881287853939368985556I ) |> should equal ( BigDecimal( 182923924147653368072110580096860218305603773162561044272625412969048I ) )

    [<Test>][<CategoryAttribute( "Big Integer Arithmetic" )>]
    let ``739275640213707404247835311064690788130735597198450568796065718887252 * 785885482647460681386967487889077852954664093400677057449596651090866`` ( ) =
        BigDecimal( 739275640213707404247835311064690788130735597198450568796065718887252I ) * BigDecimal( 785885482647460681386967487889077852954664093400677057449596651090866I ) |> should equal ( BigDecimal( 580985993318859936139832519732828292606840220831966837839307095390929399186580931070762527839814106518974824159560104749203253405461040232I ) )

    [<Test>][<CategoryAttribute( "Big Integer Arithmetic" )>]
    let ``798352755703673155609204538854615717941760641651362372040876400684717 / 570909154795050335203135447051801853399785386966877323526635322752571`` ( ) =
        BigDecimal( 798352755703673155609204538854615717941760641651362372040876400684717I ) / BigDecimal( 570909154795050335203135447051801853399785386966877323526635322752571I ) |> should equal ( BigDecimal( "1.3983884283485914589249538361991910194904424871705" ) )

    [<Test>][<CategoryAttribute( "Big Integer Arithmetic" )>]
    let ``671406175265587556711502082128429456614152875744507656162668513993535 ** 2`` ( ) =
        BigDecimal( 671406175265587556711502082128429456614152875744507656162668513993535I ) ** 2I |> should equal ( BigDecimal( 450786252184764876229067133212754695114310594211952495482186836036945126988586258988274364138696496300964747469370929452784892114021796225I ) )

    [<Test>][<CategoryAttribute( "Big Integer Arithmetic" )>]
    let ``-535574920669840191894829832231141670352658229367431700757938424316637`` ( ) =
        -BigDecimal( 535574920669840191894829832231141670352658229367431700757938424316637I ) |> should equal ( BigDecimal( -535574920669840191894829832231141670352658229367431700757938424316637I ) )

    // Basic decimal arithmetic
    [<Test>][<CategoryAttribute( "Basic Decimal Arithmetic" )>]
    let ``7.3 + 9.5`` ( ) =
        BigDecimal( 7.3 ) + BigDecimal( 9.5 ) |> should equal ( BigDecimal( 16.8 ) )

    [<Test>][<CategoryAttribute( "Basic Decimal Arithmetic" )>]
    let ``3.4 - 9.3`` ( ) =
        BigDecimal( 3.4 ) - BigDecimal( 9.3 ) |> should equal ( BigDecimal( -5.9 ) )

    [<Test>][<CategoryAttribute( "Basic Decimal Arithmetic" )>]
    let ``3.2 * 5.2`` ( ) =
        BigDecimal( 3.2 ) * BigDecimal( 5.2 ) |> should equal ( BigDecimal( 16.64 ) )

    [<Test>][<CategoryAttribute( "Basic Decimal Arithmetic" )>]
    let ``2.9 / 7.6`` ( ) =
        BigDecimal( 2.9 ) / BigDecimal( 7.6 ) |> should equal ( BigDecimal( "0.3815789473684210526315789473684210526315789473684" ) )

    [<Test>][<CategoryAttribute( "Basic Decimal Arithmetic" )>]
    let ``6.2 ** 2`` ( ) =
        BigDecimal( 6.2 ) ** 2I |> should equal ( BigDecimal( 38.44 ) )

    [<Test>][<CategoryAttribute( "Basic Decimal Arithmetic" )>]
    let ``-7.6`` ( ) =
        -BigDecimal( 7.6 ) |> should equal ( BigDecimal( -7.6 ) )

    // Big decimal arithmetic
    [<Test>][<CategoryAttribute( "Big Decimal Arithmetic" )>]
    let ``7293147747178774793.97367554367701205330016467624275349136295385064886 + 9975300791343403829.91174938656671518977490878513314002777842365721408`` ( ) =
        BigDecimal( "7293147747178774793.97367554367701205330016467624275349136295385064886" ) + BigDecimal( "9975300791343403829.91174938656671518977490878513314002777842365721408" ) |> should equal ( BigDecimal( "17268448538522178623.88542493024372724307507346137589351914137750786294" ) )

    [<Test>][<CategoryAttribute( "Big Decimal Arithmetic" )>]
    let ``6241582330368874339.56557453802295486485722377276462011786504252231568 - 5253881569270646988.74760478610052243433685322917917542987435336818789`` ( ) =
        BigDecimal( "6241582330368874339.56557453802295486485722377276462011786504252231568" ) - BigDecimal( "5253881569270646988.74760478610052243433685322917917542987435336818789" ) |> should equal ( BigDecimal( "987700761098227350.81796975192243243052037054358544468799068915412779" ) )

    [<Test>][<CategoryAttribute( "Big Decimal Arithmetic" )>]
    let ``6226428010317811326.51020659201037232660697559127778706031693118556410 * 5142722600643829854.63885306166190174480654546863908639687841094856223`` ( ) =
        BigDecimal( "6226428010317811326.51020659201037232660697559127778706031693118556410" ) * BigDecimal( "5142722600643829854.63885306166190174480654546863908639687841094856223" ) |> should equal ( BigDecimal( "32020792049943201732182218128956290750.803372810295224892660605269137608774778417363778414496294109558143795397340206818067713216226503943" ) )

    [<Test>][<CategoryAttribute( "Big Decimal Arithmetic" )>]
    let ``898257642991077743.77643781547677793244859516380034213987521570873964 / 4667640462777932760.98496497323425018885881354216167505617348835258399`` ( ) =
        BigDecimal( "898257642991077743.77643781547677793244859516380034213987521570873964" ) / BigDecimal( "4667640462777932760.98496497323425018885881354216167505617348835258399" ) |> should equal ( BigDecimal( "0.1924436233155118159905631414852881598004313977783" ) )

    [<Test>][<CategoryAttribute( "Big Decimal Arithmetic" )>]
    let ``2592777853015867689.85826019572995279085036260614484550135236795798808 ** 2`` ( ) =
        BigDecimal( "2592777853015867689.85826019572995279085036260614484550135236795798808" ) ** 2I |> should equal ( BigDecimal( "6722496995089572398685791272781571463.1751770057776423255341581051050732432547508105551158570469360667997032794193839770728493118814220864" ) )

    [<Test>][<CategoryAttribute( "Big Decimal Arithmetic" )>]
    let ``-5066451932527096033.41037336099643068303698495813071934159025338089133`` ( ) =
        -BigDecimal( "5066451932527096033.41037336099643068303698495813071934159025338089133" ) |> should equal ( BigDecimal( "-5066451932527096033.41037336099643068303698495813071934159025338089133" ) )

    // BigDecimal comparison operators
    // =
    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``88.8 = 88.8`` ( ) =
        ( BigDecimal( 88.8 ) = BigDecimal( 88.8 ) ) |> should be True

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``131.23 = 3.3`` ( ) =
        ( BigDecimal( 131.23 ) = BigDecimal( 3.3 ) ) |> should not' ( be True )

    // <>
    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``298.21 <> 2.8367`` ( ) =
        ( BigDecimal( 298.21 ) <> BigDecimal( 2.8367 ) ) |> should be True

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``5.28 <> 5.28`` ( ) =
        ( BigDecimal( 5.28 ) <> BigDecimal( 5.28 ) ) |> should not' ( be True )

    // <
    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``9.23 < 65.9`` ( ) =
        ( BigDecimal( 9.23 ) < BigDecimal( 65.9 ) ) |> should be True

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``29.87 < 13.82`` ( ) =
        ( BigDecimal( 29.87 ) < BigDecimal( 13.82 ) ) |> should not' ( be True )

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``53.2 < 53.2`` ( ) =
        ( BigDecimal( 53.2 ) < BigDecimal( 53.2 ) ) |> should not' ( be True )

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``48.75 < 0`` ( ) =
        ( BigDecimal( 48.75 ) < BigDecimal.Zero ) |> should not' ( be True )

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``191.841 < 240.623`` ( ) =
        ( BigDecimal.op_LessThan( BigDecimal( 191.841 ) , BigDecimal( 240.623 ) ) ) |> should be True

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``54.84 < 24.91`` ( ) =
        ( BigDecimal.op_LessThan( BigDecimal( 54.84 ) , BigDecimal( 24.91 ) ) ) |> should not' ( be True )

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``784.23 < 784.23`` ( ) =
        ( BigDecimal.op_LessThan( BigDecimal( 784.23 ) , BigDecimal( 784.23 ) ) ) |> should not' ( be True )

    // <=
    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``98.278 <= 101.35`` ( ) =
        ( BigDecimal( 98.278 ) <= BigDecimal( 101.35 ) ) |> should be True

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``29.1 <= 4.21`` ( ) =
        ( BigDecimal( 29.1 ) <= BigDecimal( 4.21 ) ) |> should not' ( be True )

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``28.113 <= 28.113`` ( ) =
        ( BigDecimal( 28.113 ) <= BigDecimal( 28.113 ) ) |> should be True

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``646.32 <= 0`` ( ) =
        ( BigDecimal( 646.32 ) <= BigDecimal.Zero ) |> should not' ( be True )

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``71.005 <= 113.223`` ( ) =
        ( BigDecimal.op_LessThanOrEqual( BigDecimal( 71.005 ) , BigDecimal( 113.223 ) ) ) |> should be True

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``980.73 <= 45.97`` ( ) =
        ( BigDecimal.op_LessThanOrEqual( BigDecimal( 980.73 ) , BigDecimal( 45.97 ) ) ) |> should not' ( be True )

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``4320.840 <= 4320.840`` ( ) =
        ( BigDecimal.op_LessThanOrEqual( BigDecimal( 4320.840 ) , BigDecimal( 4320.840 ) ) ) |> should be True

    // >
    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``15.6 > 9.872`` ( ) =
        ( BigDecimal( 15.6 ) > BigDecimal( 9.872 ) ) |> should be True

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``9.85 > 23.98`` ( ) =
        ( BigDecimal( 9.85 ) > BigDecimal( 23.98 ) ) |> should not' ( be True )

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``24.65 > 24.65`` ( ) =
        ( BigDecimal( 24.65 ) > BigDecimal( 24.65 ) ) |> should not' ( be True )

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``833.3 > 0`` ( ) =
        ( BigDecimal( 833.3 ) > BigDecimal.Zero ) |> should be True

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``435.642 > 12.3347`` ( ) =
        ( BigDecimal.op_GreaterThan( BigDecimal( 435.642 ) , BigDecimal( 12.3347 ) ) ) |> should be True

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``52.304 > 213.541`` ( ) =
        ( BigDecimal.op_GreaterThan( BigDecimal( 52.304 ) , BigDecimal( 213.541 ) ) ) |> should not' ( be True )

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``300.19 > 300.19`` ( ) =
        ( BigDecimal.op_GreaterThan( BigDecimal( 300.19 ) , BigDecimal( 300.19 ) ) ) |> should not' ( be True )

    // >=
    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``57.29 >= 7.52`` ( ) =
        ( BigDecimal( 57.29 ) >= BigDecimal( 7.52 ) ) |> should be True

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``3.27 >= 88.32`` ( ) =
        ( BigDecimal( 3.27 ) >= BigDecimal( 88.32 ) ) |> should not' ( be True )

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``37.3 >= 37.3`` ( ) =
        ( BigDecimal( 28.113 ) >= BigDecimal( 28.113 ) ) |> should be True

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``101.10 > 0`` ( ) =
        ( BigDecimal( 101.10 ) > BigDecimal.Zero ) |> should be True

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``8.7445 >= 31.45`` ( ) =
        ( BigDecimal.op_GreaterThanOrEqual( BigDecimal( 8.7445 ) , BigDecimal( 31.45 ) ) ) |> should be True

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``234.32 >= 724.101`` ( ) =
        ( BigDecimal.op_GreaterThanOrEqual( BigDecimal( 234.32 ) , BigDecimal( 724.101 ) ) ) |> should not' ( be True )

    [<Test>][<CategoryAttribute( "BigDecimal Comparison Operators" )>]
    let ``17.3 >= 17.3`` ( ) =
        ( BigDecimal.op_GreaterThanOrEqual( BigDecimal( 17.3 ) , BigDecimal( 17.3 ) ) ) |> should be True

    // ToString
    [<Test>]
    let ``BigDecimal( 56.8937 ).ToString( )`` ( ) =
        ( BigDecimal( 56.8937 ).ToString( ) ) |> should equal "56.8937"

    // BigDecimal functions
    // toBigInteger
    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal.toBigInteger( BigDecimal( 343 ) )`` ( ) =
        ( BigDecimal.toBigInteger( BigDecimal( 343 ) ) ) |> should equal 343I

    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal.toBigInteger( BigDecimal( 93.273 ) )`` ( ) =
        ( BigDecimal.toBigInteger( BigDecimal( 93.273 ) ) ) |> should equal 93I

    // isDecimal
    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal.isDecimal( BigDecimal( 984 ) )`` ( ) =
        ( BigDecimal.isDecimal( BigDecimal( 984 ) ) ) |> should not' ( be True )

    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal.isDecimal( BigDecimal( 345.43 ) )`` ( ) =
        ( BigDecimal.isDecimal( BigDecimal( 345.43 ) ) ) |> should be True


    // isWhole
    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal.isWhole( BigDecimal( 895 ) )`` ( ) =
        ( BigDecimal.isWhole( BigDecimal( 895 ) ) ) |> should be True

    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal.isWhole( BigDecimal( 23.479 ) )`` ( ) =
        ( BigDecimal.isWhole( BigDecimal( 23.479 ) ) ) |> should not' ( be True )


    // parse
    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal.parse( "8221" )`` ( ) =
        ( BigDecimal.parse( "8221" ) ) |> should equal ( BigDecimal( 8221 ) )

    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal.parse( "345.78" )`` ( ) =
        ( BigDecimal.parse( "345.78" ) ) |> should equal ( BigDecimal( 345.78 ) )

    // pow
    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal( 412 ) |> BigDecimal.pow 38I`` ( ) =
        ( BigDecimal( 412 ) |> BigDecimal.pow 38I ) |> should equal ( BigDecimal( 2323240710308686152754130835569739812958045579460117063631548848379628135903309146400747732812693504I ) )

    // nthrt
    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal( 81 ) |> BigDecimal.nthrt 2`` ( ) =
        ( BigDecimal( 81 ) |> BigDecimal.nthrt 2 ) |> should equal ( BigDecimal( 9 ) )

    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal( 9167486769200391580986609275853801624831066801443086224071265164279346570408670965932792057674808067900227830163549248523803357453169351119035965775473400756816883056208210161291328455648I ) |> BigDecimal.nthrt 23`` ( ) =
        ( BigDecimal( 9167486769200391580986609275853801624831066801443086224071265164279346570408670965932792057674808067900227830163549248523803357453169351119035965775473400756816883056208210161291328455648I ) |> BigDecimal.nthrt 23 ) |> should equal ( BigDecimal( "134522054.33137513807694048238669278806693778558635" ) )

    // sqrt
    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal( 64 ) |> BigDecimal.sqrt`` ( ) =
        ( BigDecimal( 64 ) |> BigDecimal.sqrt ) |> should equal ( BigDecimal( 8 ) )

    // cbrt
    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal( 125 ) |> BigDecimal.cbrt`` ( ) =
        ( BigDecimal( 125 ) |> BigDecimal.cbrt ) |> should equal ( BigDecimal( 5 ) )

    // abs
    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal( 203 ) |> BigDecimal.abs`` ( ) =
        ( BigDecimal( 203 ) |> BigDecimal.abs ) |> should equal ( BigDecimal( 203 ) )

    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal( -434 ) |> BigDecimal.abs`` ( ) =
        ( BigDecimal( -434 ) |> BigDecimal.abs ) |> should equal ( BigDecimal( 434 ) )

    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal( 49.738 ) |> BigDecimal.abs`` ( ) =
        ( BigDecimal( 49.738 ) |> BigDecimal.abs ) |> should equal ( BigDecimal( 49.738 ) )

    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal( -52.03 ) |> BigDecimal.abs`` ( ) =
        ( BigDecimal( -52.03 ) |> BigDecimal.abs ) |> should equal ( BigDecimal( 52.03 ) )

    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal( 0 ) |> BigDecimal.abs`` ( ) =
        ( BigDecimal( 0 ) |> BigDecimal.abs ) |> should equal ( BigDecimal( 0 ) )

    // floor
    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal( 46.3 ) |> BigDecimal.floor`` ( ) =
        ( BigDecimal( 46.3 ) |> BigDecimal.floor ) |> should equal ( BigDecimal( 46 ) )

    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal( 20.9 ) |> BigDecimal.floor`` ( ) =
        ( BigDecimal( 20.9 ) |> BigDecimal.floor ) |> should equal ( BigDecimal( 20 ) )

    // ceil
    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal( 87.3 ) |> BigDecimal.ceil`` ( ) =
        ( BigDecimal( 87.3 ) |> BigDecimal.ceil ) |> should equal ( BigDecimal( 88 ) )

    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal( 10.85 ) |> BigDecimal.ceil`` ( ) =
        ( BigDecimal( 10.85 ) |> BigDecimal.ceil ) |> should equal ( BigDecimal( 11 ) )

    // round
    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal( 98.237 ) |> BigDecimal.round`` ( ) =
        ( BigDecimal( 98.237 ) |> BigDecimal.round 2 ) |> should equal ( BigDecimal( 98.24 ) )

    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal( 1.11 ) |> BigDecimal.round`` ( ) =
        ( BigDecimal( 1.11 ) |> BigDecimal.round 1 ) |> should equal ( BigDecimal( 1.1 ) )

    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal( 0 ) |> BigDecimal.round`` ( ) =
        ( BigDecimal( 0 ) |> BigDecimal.round 80 ) |> should equal ( BigDecimal( 0 ) )

    // whole
    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal( 349.34 ) |> BigDecimal.whole`` ( ) =
        ( BigDecimal( 349.34 ) |> BigDecimal.whole ) |> should equal ( 349I )

    // fractional
    [<Test>][<CategoryAttribute( "BigDecimal Functions" )>]
    let ``BigDecimal( 29.7492 ) |> BigDecimal.fractional`` ( ) =
        ( BigDecimal( 29.7492 ) |> BigDecimal.fractional ) |> should equal ( BigDecimal( 0.7492 ) )