unit RatCVSModel;
// -------------
// Tissue models
// -------------
// 28.01.22 Model code moved from ObSImMain to ObSimModel
// 18.04.22 Rat diaphragm model (from Twitch simulation) added
// 02.01.22 Model copied from V3.3.7 (last VCL version)
// 28.8.23  Captopril EC50 Reduced from 5 -> 1 28.8.23

interface

uses
  System.SysUtils, System.Classes, System.Math, System.strutils, FMX.ListBox, fmx.types ;

const

    NumChannels = 5 ; // No. of chart traces
    ChABP = 0 ;   { Arterial blood pressure channel }
    ChLVP = 1 ;   { Average bloood pressure channel }
    ChCVP = 2 ;   { Venous blood pressure }
    ChHCF = 3 ;   { Cardiac contractility }
    ChHR = 4 ;    { Heart rate channel }

    None = -1. ;  // No Potency flag
    MaxDrugs = 100 ;
    // Drug available for applying to tissue flags

    dtAgonist = 0 ;
    dtAntagonist = 1 ;
    dtUnknown = 3 ;
    dtNerve = 4 ;

    BackgroundNoiseStDev = 0.1 ;  // Background noise (gms)
    ForceStDev = 0.05 ;
    MaxMixingRate = 0.5 ;
    MeanRMax = 15.0 ;
    RMaxStDev = 0.05 ;
    cBathVolume = 10.0 ;          // Organ bath volume (ml)
    ReservoirVolume = 1000.0 ;   // Krebs solution reservoir volume (ml)
//    dt = 0.15 ;                    // Simulation time step (s)

type

    TDrugProperties = record
                    Potency : single ;
                    Efficacy : single ;
                    end ;

    TDrug = class(TObject)
            public
            Name : string[40] ;
            ShortName : string ;
            Dose : single ;
            DoseInjected : single ;
            Conc : single ;
            Alpha1AdR : TDrugProperties ;
            Alpha2AdR : TDrugProperties ;
            Beta1AdR : TDrugProperties ;
            Beta2AdR : TDrugProperties ;
            MusChR : TDrugProperties ;
            VagChR : TDrugProperties ;
            NicChR : TDrugProperties ;
            HMCaChannel : TDrugProperties ;
            SMCaChannel : TDrugProperties ;
            AdenR : TDrugProperties ;
            KChannel : TDrugProperties ;
            NOX : TDrugProperties ;
            NOS : TDrugProperties ;
            Angt1R: TDrugProperties ;
            Angt2R: TDrugProperties ;
            BradR: TDrugProperties ;
            ACE : TDrugProperties ;
            DigR : TDrugProperties ;
            PDE : TDrugProperties ;
            OnRate : single ;
            RemovalRate : single ;
            MinDose : single ;
            MaxDose : single ;
            DrugType : Integer ;
            constructor Create(
                        NameIn : String ;
                        ShortNameIn : string
                        ) ;
            end ;

    TElement = class(TObject)
               public
               Conductance : single ;
               Elastance : single ;
               Pressure : single ;
               MaxPressure : single ;
               MinPressure : single ;
               MeanPressure : single ;
               PressureLo : single ;
               PressureHi : single ;
               TLo : single ;
               THi : single ;
               Volume : single ;
               Volume0 : single ;
               Flow : Single ;
               Rate : single ;
               TSystole : single ;
               NextBeat : single ;
               Time : single ;
               Force : single ;
               procedure CalculatePressureRange( t : single ) ;
               procedure InitialisePressureRange ;
               function GetLeadingPressure : single ;
               function GetTrailingPressure : single ;
               end ;

    TTime = record
          time : single ;
          diastole : single ;
          systole : single ;
          step : single ;
          next : single ;
          end ;

    TBP = record
        diastolic : single ;
        systolic : single ;
        mean : single ;
        value : single ;
        end ;

    TNerve = record
           Stimulated : boolean ;
           StimulusInterval : single ;
           StimulusActivity : single ;
           Activity : single ;
           end ;


  TModel = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }

    FModels : TStringList ; // Names of models available

   Dying : LongInt ;
   Fibrillation : single ;
   Ven : TElement ;
   Art : TElement ;
   Heart : TElement ;
   Con : TElement ;

   procedure SetAgonistPotency(
              var Drug : TDrugProperties ;
              Potency : single
              ) ;

   procedure SetAntagonistPotency(
              var Drug : TDrugProperties ;
              Potency : single
              ) ;

    Function ReceptorActivation(
          iDrug : Integer ;
          Dose : single ;
          var Drug : TDrugProperties ;
          var Numerator : single ;
          var Denominator : single ;
          PotencyShift : single
          ) : single ;


    function Pressure( Element : TElement ) : single ;
    function ExtractFloat ( CBuf : string ; Default : Single ) : extended ;

  public
    { Public declarations }
    NormalRat : Boolean ;                               // TRUE = normal rat with central cvs reflexes
                                                        // FALSE = Pithed rat
    t : TTime ;                                        // Elapsed time since start of simulation

    Drugs : Array[0..MaxDrugs-1] of TDrug ;            // Drug data

    // Chart trace channels
    ChanNames : Array[0..NumChannels-1] of string  ;      // Channel names
    ChanUnits : Array[0..NumChannels-1] of string  ;      // Channel units
    ChanValues : Array[0..NumChannels*2-1] of single  ;   // Channel values (pair of time points)

    NumDrugs : Integer ;                                // No. drugs available

    InitialMixing : Cardinal ;

   // Index into Drugs[] of neurtransmitters activated by selected nerve pathways
   iSympHeart : Integer ;         // Sympathetic stimulation of heart
   iSympBloodVessels : Integer ;  // Sympathetic stimulation of blood vessels
   iSympAdrenal : Integer ;       // Sympathetic stimulation of adrenal glands
   iRenalNerve : Integer ;        // Renal nerve stimulation
   iVagusNerve : Integer ;        // Vagus nerve stimulation

   MusChR : single ; { Proportion of muscarinic cholinoceptors activated }
   VagChR : single ; { Proportion of muscarinic cholinoceptors activated }
   NicChR : single ; { Proportion of nicotinic cholinoceptors activated }
   Alpha1AdR : single ; { Proportion of Alpha1 adrenoceptors activated }
   iAlpha1AdrNerves : Integer ;
   Alpha2AdR : single ; { Proportion of Alpha2 adrenoceptors activated }
   Beta1AdR : single ; { Proportion of beta-1 adrenoceptors activated }
   iBeta1AdrNerves : Integer ;
   Beta2AdR : single ; { Proportion of beta-2 adrenoceptors activated }
   AdenR : single ; { Proportion of adenosine receptors activated }
   HMCaChannels : single ; { Proportion of heart muscle Ca channels blocked }
   SMCaChannels : single ; { Proportion of smooth muscle Ca channels blocked }
   KChannels : single ; { K channel mediated vasodilation }
   NOX : single ; { Nitric oxide mediated vasodilation }
   NOS : single ; { Nitric oxide synthase }
   Angt1R : single ; { Angiotensin I receptor }
   iAngiotensin1 : Integer ; { Angiotensin I entry with Drugs array }
   Angt2R : single ; { Angiotensin II receptor }
   iAngiotensin2ACE : Integer ; { ACE-produced Angiotensin II entry with Drugs array }

   ACE : single ;    { Angiotensin converting enzyme activity }
   BradR : single ;  { Bradykinin recptor }
   DigR : single ;   { Digoxin activity on cardiac force }
   PDE : single ; { Degree of phosphidesterase inhibition }

   Dead : boolean ;    // Rat is dead

    procedure InitialiseModel ;
    procedure DoSimulationStep ;

    procedure GetListOfDrugs(
              DrugList : TComboBox ;         // Return list of drugs
              DrugType : Integer ) ;         // Type of drug (Agonist,Antagonist,Unknown)

    function AddNoise( Proportion : single ) : single ;

  end;

var
  Model: TModel;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

procedure TModel.DataModuleCreate(Sender: TObject);
// --------------------------------------
// Initialisations when module is created
// --------------------------------------
begin

    FModels := TStringList.Create ;

     // Create circulation elements

     Heart := TElement.Create ;
     Art := TElement.Create ;
     Ven := TElement.Create ;
     Con := TElement.Create ;

    // Output channels

     ChanNames[ChABP] := 'ABP' ;
     ChanUnits[ChABP] := 'mmHg' ;

     { Left ventricular pressure }
     ChanNames[chLVP] := 'LVP' ;
     ChanUnits[chLVP] := 'mmHg' ;

     { Venous Pressure }
     ChanNames[chCVP] := 'VBP' ;
     ChanUnits[chCVP] := 'mmHg' ;

     { Cardiac contractile force (dLVP/dt)/LVP }
     ChanNames[chHCF] := 'HF' ;
     ChanUnits[chHCF] := '' ;

     { Heart rate }
     ChanNames[chHR] := 'HR' ;
     ChanUnits[chHR] := 'BPM' ;

end;


procedure TModel.DataModuleDestroy(Sender: TObject);
begin
     Heart.Free ;
     Art.Free ;
     Ven.Free ;
     Con.Free ;

end;

procedure TModel.GetListOfDrugs(
          DrugList : TComboBox ;         // Return list of drugs
          DrugType : Integer ) ;        // Type of drug (Agonist,Antagonist,Unknown)
// ---------------------------------------
// Return list of drugs of specified type
// ---------------------------------------
var
    i : Integer ;
begin

     DrugList.Clear ;

     for i := 0 to NumDrugs-1 do
         if DrugType = Drugs[i].DrugType then
            begin
            DrugList.Items.AddObject( Drugs[i].Name, TObject(i)) ;
         end;
     end ;


procedure TModel.InitialiseModel ;
// ---------------------------------------
// Initialise model to starting conditions
// ---------------------------------------
{ ---------------------------------------------
  Set up initial conditions for simulation
  15/10/97 Better temporary file allocation
  ----------------------------------------}

const
     pFilePrefix : PChar = 'RCV' ;
     FastOn = 0.006 ;
     FastOff = 0.0035 ;
     SlowOn = 0.002 ;
     SlowOff = 5E-4 ;

var
   iDrug : Integer ;
begin

     {Initialise drug doses and potencies }

     for iDrug := 0 to High(Drugs) do Drugs[iDrug] := Nil ;

{    *** STANDARD DRUGS ***************************************************}

     { Adrenaline - Alpha11,beta1,beta2 adrenoceptor agonist }
     iDrug := 0 ;
     Drugs[iDrug] := TDrug.Create( 'Adrenaline', 'Adr' ) ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := FastOff ;
     SetAgonistPotency( Drugs[iDrug].Alpha1AdR, 5E-3 {7.5e-3} ) ;  {Increased to 7.5E-3 to cause increase in ABP at high concentrations 22.7.18 }
                                                                   // Restored to previous 5E-3 to increase alpha adrenoecrptor effect
                                                                   // to ensure increase in ABP with 5-10 ug/kg as observed in original pithed rat paper 19.06.19
     SetAgonistPotency( Drugs[iDrug].Beta1AdR, 3.5E-3 ) ;
     SetAgonistPotency( Drugs[iDrug].Beta2AdR, 5E-3 {2E-3} ) ; {Reduced to 2E-3 to cause reduction in ABP at low concentrations 22.7.18}
                                                               // Restored to previous value 19.06.19

     Drugs[iDrug].MinDose := 1E-4 ;
     Drugs[iDrug].MaxDose := 1E-1 ;

//   Agonists
//   --------

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create( 'Noradrenaline', 'Nor' ) ;
     Drugs[iDrug].DrugType := dtAgonist ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := FastOff ;
     SetAgonistPotency( Drugs[iDrug].Alpha1AdR, 1E-2 ) ;
     SetAgonistPotency( Drugs[iDrug].Beta1AdR, 1.5E-2 ) ;
     SetAgonistPotency( Drugs[iDrug].Beta2AdR, 1.5E-1 ) ;
     Drugs[iDrug].MinDose := 1E-3 ;
     Drugs[iDrug].MaxDose := 1.0 ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Isoprenaline', 'Iso' ) ;
     Drugs[iDrug].DrugType := dtAgonist ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := FastOff ;
     SetAgonistPotency( Drugs[iDrug].Beta1AdR, 1E-3 ) ;
     SetAgonistPotency( Drugs[iDrug].Beta2AdR, 7E-4 ) ;
     Drugs[iDrug].MinDose := 1E-4 ;
     Drugs[iDrug].MaxDose := 1E-1 ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Phenylephrine', 'Phe' ) ;
     Drugs[iDrug].DrugType := dtAgonist ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := FastOff ;
     SetAgonistPotency( Drugs[iDrug].Alpha1AdR, 1E-2 ) ;
     Drugs[iDrug].MinDose := 1E-3 ;
     Drugs[iDrug].MaxDose := 1. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Acetylcholine', 'Ach' ) ;
     Drugs[iDrug].DrugType := dtAgonist ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := FastOff*2.0 ;
     SetAgonistPotency( Drugs[iDrug].MusChR, 5E-3 ) ;
     SetAgonistPotency( Drugs[iDrug].NicChR, 0.15 ) ;
     SetAgonistPotency( Drugs[iDrug].Alpha1AdR, 0.3 ) ;
     SetAgonistPotency( Drugs[iDrug].Beta1AdR, 0.15 ) ;
     Drugs[iDrug].MinDose := 1E-3 ;
     Drugs[iDrug].MaxDose := 1. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Adenosine', 'Ade' ) ;
     Drugs[iDrug].DrugType := dtAgonist ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := FastOff ;
     SetAgonistPotency( Drugs[iDrug].AdenR, 1.0 ) ;
     Drugs[iDrug].MinDose := 0.1 ;
     Drugs[iDrug].MaxDose := 100. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Cromakalim', 'Cro' ) ;     { K channel opener }
     Drugs[iDrug].DrugType := dtAgonist ;
     Drugs[iDrug].OnRate := SlowOn ;                            {causes vasodilation }
     Drugs[iDrug].RemovalRate := SlowOff ;
     SetAgonistPotency( Drugs[iDrug].KChannel, 0.3 ) ;
     Drugs[iDrug].MinDose := 0.1 ;
     Drugs[iDrug].MaxDose := 100. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Angiotensin I', 'An1' ) ;
     Drugs[iDrug].DrugType := dtAgonist ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := FastOff ;
     SetAgonistPotency( Drugs[iDrug].Angt1R, 1E-4 ) ;
     iAngioTensin1 := iDrug ;
     Drugs[iDrug].MinDose := 1E-4 ;
     Drugs[iDrug].MaxDose := 1E-2 ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Angiotensin II', 'An2' ) ;    // Agonist at Angiotensin II receptors
     Drugs[iDrug].DrugType := dtAgonist ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := FastOff ;
     SetAgonistPotency( Drugs[iDrug].Angt2R, 1E-4 ) ;
     Drugs[iDrug].MinDose := 1E-4 ;
     Drugs[iDrug].MaxDose := 1E-2 ;

//   Antagonists
//   -----------

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Propranolol', 'Pro' ) ;  { Beta Adr. }
     Drugs[iDrug].DrugType := dtAntagonist ;
     Drugs[iDrug].OnRate := FastOn ;                         { Antagonist }
     Drugs[iDrug].RemovalRate := SlowOff ;                   { Potency modified 4/7/00}
     SetAntagonistPotency( Drugs[iDrug].Beta1AdR, 0.5 ) ;
     SetAntagonistPotency( Drugs[iDrug].Beta2AdR, 0.5 ) ;
     Drugs[iDrug].MinDose := 0.01 ;
     Drugs[iDrug].MaxDose := 100. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Atenolol', 'Ate' ) ;     { Beta-1 Adr. }
     Drugs[iDrug].DrugType := dtAntagonist ;
     Drugs[iDrug].OnRate := FastOn ;                          { Antagonist }
     Drugs[iDrug].RemovalRate := SlowOff ;
     SetAntagonistPotency( Drugs[iDrug].Beta1AdR, 10.0 ) ;
     Drugs[iDrug].MinDose := 1.0 ;
     Drugs[iDrug].MaxDose := 1000. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Phentolamine', 'Phe' ) ;  { Alpha Adr. }
     Drugs[iDrug].DrugType := dtAntagonist ;
     Drugs[iDrug].OnRate := FastOn ;                           { antagonist }
     Drugs[iDrug].RemovalRate := SlowOff ;
     SetAntagonistPotency( Drugs[iDrug].Alpha1Adr, 1E-1 ) ;
     SetAntagonistPotency( Drugs[iDrug].Alpha2Adr, 1E-1 ) ;
     Drugs[iDrug].MinDose := 1E-2 ;
     Drugs[iDrug].MaxDose := 10. ;


     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Prazosin', 'Pra' ) ;    { Alpha-1 Adr. }
     Drugs[iDrug].DrugType := dtAntagonist ;
     Drugs[iDrug].OnRate := FastOn ;                         { antagonist }
     Drugs[iDrug].RemovalRate := SlowOff ;
     SetAntagonistPotency( Drugs[iDrug].Alpha1Adr, 0.1 ) ;
     Drugs[iDrug].MinDose := 0.01 ;
     Drugs[iDrug].MaxDose := 10. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Atropine', 'Atr' ) ;
     Drugs[iDrug].DrugType := dtAntagonist ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := SlowOff ;
     SetAntagonistPotency( Drugs[iDrug].MusChR, 0.1 ) ; {Potency reduced to 0.1 4/7/00}
     SetAntagonistPotency( Drugs[iDrug].VagChR, 0.1 ) ;
     Drugs[iDrug].MinDose := 0.01 ;
     Drugs[iDrug].MaxDose := 10.0 ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  '8-s-p-theophylline', '8spt' ) ;
     Drugs[iDrug].DrugType := dtAntagonist ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := SlowOff ;
     SetAntagonistPotency( Drugs[iDrug].AdenR, 5.0 ) ;
     Drugs[iDrug].MinDose := 0.1 ;
     Drugs[iDrug].MaxDose := 100.0 ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Verapamil', 'Ver' ) ;
     Drugs[iDrug].DrugType := dtAntagonist ;
     Drugs[iDrug].OnRate := SlowOn ;
     Drugs[iDrug].RemovalRate := SlowOff ;
     SetAgonistPotency( Drugs[iDrug].HMCaChannel, 0.75 ) ;
     SetAgonistPotency( Drugs[iDrug].SMCaChannel, 0.75 ) ;
     Drugs[iDrug].MinDose := 0.1 ;
     Drugs[iDrug].MaxDose := 100. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'L-NOARG', 'LNO' ) ;    // Antagonist of nitric oxide synthase
     Drugs[iDrug].DrugType := dtAntagonist ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := SlowOff ;
     SetAgonistPotency( Drugs[iDrug].NOS, 10.0 ) ;
     Drugs[iDrug].MinDose := 1.0 ;
     Drugs[iDrug].MaxDose := 1000. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Captopril', 'Cap' ) ; { ACE inhibitor }
     Drugs[iDrug].DrugType := dtAntagonist ;
     Drugs[iDrug].OnRate := SlowOn ;
     Drugs[iDrug].RemovalRate := SlowOff ;
     SetAgonistPotency( Drugs[iDrug].ACE, 1.0 ) ;    // Reduced from 5 -> 1 28.8.23
     //SetAgonistPotency( Drugs[iDrug].BradR, 5.0 ) ;
     Drugs[iDrug].MinDose := 1.0 ;
     Drugs[iDrug].MaxDose := 1000. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Losartan', 'Los' ) ; { Blocks angiotensin II }
     Drugs[iDrug].DrugType := dtAntagonist ;
     Drugs[iDrug].OnRate := FastOn ; ;
     Drugs[iDrug].RemovalRate := SlowOff ;
     SetAntagonistPotency( Drugs[iDrug].Angt2R, 1.0 ) ;    { Potency reduced to 1 4/7/00) }
     Drugs[iDrug].MinDose := 1. ;
     Drugs[iDrug].MaxDose := 1000. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Digoxin', 'Dig' ) ; { Digoxin }
     Drugs[iDrug].DrugType := dtAntagonist ;
     Drugs[iDrug].OnRate := SlowOn*0.25 ;
     Drugs[iDrug].RemovalRate := SlowOff ;
     SetAgonistPotency( Drugs[iDrug].DigR, 0.5 ) ;
     Drugs[iDrug].MinDose := 0.1 ;
     Drugs[iDrug].MaxDose := 100. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Glyceryl trinitrate', 'GTN' ) ;     // Glyceryl trinitrate - NO donor - treated as agonist
     Drugs[iDrug].DrugType := dtAgonist ;                                // Now included in agonist list 23.2.24
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := FastOff ;
     SetAgonistPotency( Drugs[iDrug].NOX, 1E-2 ) ;                       // EC50 reduced 1000 fold 22.2.24
     Drugs[iDrug].MinDose := 1E-3 ;
     Drugs[iDrug].MaxDose := 1.0 ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Milrinone', 'Mil' ) ;     { Phosphodiesterase III inh. }
     Drugs[iDrug].DrugType := dtAntagonist ;                   // Increases HR, decreases PVR
     Drugs[iDrug].OnRate := SlowOn ;
     Drugs[iDrug].RemovalRate := SlowOff ;
     SetAgonistPotency( Drugs[iDrug].PDE, 1.0 ) ;              // Reduced 10->1 mg/kg 23.2.24
     Drugs[iDrug].MinDose := 1. ;
     Drugs[iDrug].MaxDose := 1000. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Glibenclamide', 'Gli' ) ;   { K Channel blocker }
     Drugs[iDrug].DrugType := dtAntagonist ;
     Drugs[iDrug].OnRate := SlowOn ;
     Drugs[iDrug].RemovalRate := SlowOff ;
     SetAntagonistPotency( Drugs[iDrug].KChannel, 1.0 ) ;
     Drugs[iDrug].MinDose := 0.1 ;
     Drugs[iDrug].MaxDose := 100. ;


//   UNKNOWN DRUGS
//   -------------

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Drug A', 'A' ) ;    { Acetylcholine }
     Drugs[iDrug].DrugType := dtUnknown ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := FastOff ;
     SetAgonistPotency( Drugs[iDrug].MusChR, 5E-3 ) ;
     SetAgonistPotency( Drugs[iDrug].NicChR, 0.1 ) ;
     SetAgonistPotency( Drugs[iDrug].Alpha1AdR, 0.3 ) ;
     SetAgonistPotency( Drugs[iDrug].Beta1AdR, 0.15 ) ;
     Drugs[iDrug].MinDose := 1E-3 ;
     Drugs[iDrug].MaxDose := 1. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Drug B', 'B' ) ;     { Adenosine  }
     Drugs[iDrug].DrugType := dtUnknown ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := FastOff ;
     SetAgonistPotency( Drugs[iDrug].AdenR, 1.0 ) ;
     Drugs[iDrug].MinDose := 0.1 ;
     Drugs[iDrug].MaxDose := 1000. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Drug C', 'C' ) ;    { Isoprenaline }
     Drugs[iDrug].DrugType := dtUnknown ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := SlowOff ;
     SetAgonistPotency( Drugs[iDrug].Beta1AdR, 1E-3 ) ;
     SetAgonistPotency( Drugs[iDrug].Beta2AdR, 7E-4 ) ;
     Drugs[iDrug].MinDose := 1E-4 ;
     Drugs[iDrug].MaxDose := 1E-1 ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Drug D', 'D' ) ;    { Noradrenaline }
     Drugs[iDrug].DrugType := dtUnknown ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := FastOff ;
     SetAgonistPotency( Drugs[iDrug].Alpha1AdR, 1E-2 ) ;
     SetAgonistPotency( Drugs[iDrug].Beta1AdR, 1.5E-2 ) ;
     SetAgonistPotency( Drugs[iDrug].Beta2AdR, 1.5E-1 ) ;
     Drugs[iDrug].MinDose := 1E-3 ;
     Drugs[iDrug].MaxDose := 1. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create( 'Drug E', 'E' ) ;      { Sodium nitroprusside }
     Drugs[iDrug].DrugType := dtUnknown ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := FastOff ;
     SetAgonistPotency( Drugs[iDrug].NOX, 15.0 ) ;
     Drugs[iDrug].MinDose := 1.0 ;
     Drugs[iDrug].MaxDose := 1000. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create( 'Drug F', 'F' ) ;    { Cromokalim }
     Drugs[iDrug].DrugType := dtUnknown ;
     Drugs[iDrug].OnRate := SlowOn ;
     Drugs[iDrug].RemovalRate := SlowOff ;
     SetAgonistPotency( Drugs[iDrug].KChannel, 0.3 ) ;
     Drugs[iDrug].MinDose := 1.0 ;
     Drugs[iDrug].MaxDose := 1000. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Drug G', 'G' ) ;     { Milrinone }
     Drugs[iDrug].DrugType := dtUnknown ;
     Drugs[iDrug].OnRate := SlowOn ;
     Drugs[iDrug].RemovalRate := SlowOff ;
     SetAgonistPotency( Drugs[iDrug].PDE, 10.0 ) ;
     Drugs[iDrug].MinDose := 1. ;
     Drugs[iDrug].MaxDose := 1000. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Drug H', 'H' ) ;     { Propanalol }
     Drugs[iDrug].DrugType := dtUnknown ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := SlowOff ;
     SetAntagonistPotency( Drugs[iDrug].Beta1AdR, 10.0 ) ;
     SetAntagonistPotency( Drugs[iDrug].Beta2AdR, 10.0 ) ;
     Drugs[iDrug].MinDose := 1.0 ;
     Drugs[iDrug].MaxDose := 1000. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Drug I', 'I' ) ;     { Verapamil }
     Drugs[iDrug].DrugType := dtUnknown ;
     Drugs[iDrug].OnRate := SlowOn ;
     Drugs[iDrug].RemovalRate := SlowOff ;
     SetAgonistPotency( Drugs[iDrug].HMCaChannel, 0.75 ) ;
     SetAgonistPotency( Drugs[iDrug].SMCaChannel, 0.75 ) ;
     Drugs[iDrug].MinDose := 0.1 ;
     Drugs[iDrug].MaxDose := 100. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Drug J', 'J' ) ;   { L-NOARG (NOS-inhibitor) }
     Drugs[iDrug].DrugType := dtUnknown ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := SlowOff ;
     SetAgonistPotency( Drugs[iDrug].NOS, 10.0 ) ;
     Drugs[iDrug].MinDose := 1. ;
     Drugs[iDrug].MaxDose := 1000. ;

     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Drug K', 'K' ) ;   { Atropine }
     Drugs[iDrug].DrugType := dtUnknown ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := SlowOff ;
     SetAntagonistPotency( Drugs[iDrug].MusChR, 0.1 ) ; {Potency reduced to 0.1 4/7/00}
     SetAntagonistPotency( Drugs[iDrug].VagChR, 0.1 ) ;
     Drugs[iDrug].MinDose := 0.1 ;
     Drugs[iDrug].MaxDose := 100. ;

{    *** NERVE STIMULATION **************************************************}

     { Cardiac nerve stimulation - beta1 adrenoceptor agonist }
     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Cardiac', 'Symp(H)' ) ;
     Drugs[iDrug].DrugType := dtNerve ;
     Drugs[iDrug].OnRate := FastOn*4.0 ;
     Drugs[iDrug].RemovalRate := FastOff*4.0 ;
     SetAgonistPotency( Drugs[iDrug].Beta1AdR, 1.5E-2 ) ;

     { Adrenal nerve stimulation - Alpha1,beta1,beta2 adrenoceptor agonist }
     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'AdrenNerve', 'Symp(AD)' ) ;
     Drugs[iDrug].DrugType := dtNerve ;
     Drugs[iDrug].OnRate := FastOn*2.0 ;
     Drugs[iDrug].RemovalRate := FastOff ;
     SetAgonistPotency( Drugs[iDrug].Alpha1AdR, 5E-3 ) ;
     SetAgonistPotency( Drugs[iDrug].Beta1AdR, 3.55E-3 ) ;
     SetAgonistPotency( Drugs[iDrug].Beta2AdR, 5E-3 ) ;
     iSympAdrenal := iDrug ;

     { Blood vessel symp. nerve stimulation - noradrenaline }
     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'SympNerve', 'Symp(-AD)' ) ;
     Drugs[iDrug].DrugType := dtNerve ;
     Drugs[iDrug].OnRate := FastOn*7.0 ;
     Drugs[iDrug].RemovalRate := FastOff*4.0 ;
     SetAgonistPotency( Drugs[iDrug].Alpha1AdR, 1E-2 ) ;
     SetAgonistPotency( Drugs[iDrug].Beta1AdR, 1.5E-2 ) ;
     SetAgonistPotency( Drugs[iDrug].Beta2AdR, 1.5E-1 ) ;
     iSympBloodVessels := iDrug ;

     { Vagus nerve stimulation - cholinergic agonist }
     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Vagus', 'Vag. Nv.' ) ;
     Drugs[iDrug].DrugType := dtNerve ;
     Drugs[iDrug].OnRate := FastOn*4.0 ;
     Drugs[iDrug].RemovalRate := FastOn*4.0 ;
     SetAgonistPotency( Drugs[iDrug].VagChR, 5E-3 ) ;
     iVagusNerve := iDrug ;

      { Renal nerve stimulation - Release angiotensin I - vasoconstrictor }
     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Renal', 'Ren. Nv.' ) ;
     Drugs[iDrug].DrugType := dtNerve ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := FastOff ;
     SetAgonistPotency( Drugs[iDrug].Angt1R, 2E-4 ) ;
     iRenalNerve := iDrug ;

     { Vascular nerve stimulation acting on alpha-1 adrenoceptors }
     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Alpha1', '???' ) ;
     Drugs[iDrug].DrugType := dtNerve ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := FastOff*2.0 ;
     SetAgonistPotency( Drugs[iDrug].Alpha1AdR, 1E-2 ) ;
     iAlpha1AdrNerves := iDrug ;

     { Nerve stimulation acting on beta-1 adrenoceptors in heart}
     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'Beta1', '???' ) ;
     Drugs[iDrug].DrugType := dtNerve ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := FastOff*2.0 ;
     SetAgonistPotency( Drugs[iDrug].Beta1AdR, 1E-2 ) ;
     iBeta1AdrNerves := iDrug ;

     // ACE produced Angiotensin II via renal nerve stimulation
     Inc(iDrug) ;
     Drugs[iDrug] := TDrug.Create(  'AngiotensinIIACE', 'An2' ) ;
     Drugs[iDrug].DrugType := dtNerve ;
     Drugs[iDrug].OnRate := FastOn ;
     Drugs[iDrug].RemovalRate := FastOff ;
     SetAgonistPotency( Drugs[iDrug].Angt2R, 1E-4 ) ;
     iAngioTensin2ACE := iDrug ;
     Drugs[iDrug].MinDose := 1E-4 ;
     Drugs[iDrug].MaxDose := 1E-2 ;

     Inc(iDrug) ;
     NumDrugs := iDrug ;

     for iDrug := 0 to NumDrugs-1 do Drugs[iDrug].Dose := 0.0 ;
     for iDrug := 0 to NumDrugs-1 do Drugs[iDrug].DoseInjected := 0.0 ;

     Heart.Elastance := 1./{75.} 30.0 ;
     Heart.Volume0 := 5. ;
     Heart.Volume := 10. ;
     Heart.Conductance := 2500. ;
     Heart.NextBeat := 0.0 ;
     Heart.Rate := 400.0 ;

     Art.Conductance := 0.05 ;
     Art.Elastance := 1./0.8;
     Art.Volume0 := 100.0 ;
     Art.Volume := 150.0 ;

     Ven.Conductance := 5000.0 ;
     Ven.Elastance := 1./ 30.0 ;
     Ven.Volume0 := 300. ;
     Ven.Volume := 600. ;


     t.Step := 0.125E-3;//0.125E-3 ;
     t.time := 0. ;
     t.next := -1.0 ;


     Dead := False ;
     Dying := 0 ;
     Fibrillation := 0.0 ;

     end ;


procedure TModel.DoSimulationStep  ;
{ -------------------------------------------
  Calculate next step of simulated BP and HR
  ------------------------------------------}
const
    NumStepsPerDisplayPoint = 100 ;
var
   x : single ;
   i : Integer ;
   Num,Denom : single ;
   PotencyShift,TMid,YMax,YMin : single ;
   AchFromVagus : Single ;
begin

     if t.time >= t.next then
        begin

        { Ensure resting levels of angiotensin in circulation }
        Drugs[iRenalNerve].Dose := Max( Drugs[iRenalNerve].Dose,
                                        Drugs[iRenalNerve].Angt1R.Potency*0.25) ;

        { Nicotinic (ganglion) cholinoceptor activation }
        for i := 0 to High(Drugs) do if Drugs[i] <> Nil then
            NicChR := ReceptorActivation(i,Drugs[i].Dose,Drugs[i].NicChR,Num,Denom,1.0 ) ;

        { Effects of sympathetic nerve and vagal baroreceptor feedback
          in normal UNPITHED rat }

        if NormalRat then
           begin

           { Basal sympathetic nerve stimulation of blood vessels }
           Drugs[iAlpha1AdrNerves].Dose := Drugs[iAlpha1AdrNerves].Alpha1Adr.Potency*0.4 ;
           Drugs[iBeta1AdrNerves].Dose := Drugs[iBeta1AdrNerves].Beta1Adr.Potency*0.2 ;

           { ** Baroreceptor feedback loops (in normal rat only) ** }
           AchFromVagus := Drugs[iVagusNerve].VagChR.Potency* (5.0 / (1.+ exp(-(Art.MeanPressure-130.0)/8.0))) ;
           Drugs[iVagusNerve].Dose := Max( Drugs[iVagusNerve].Dose,AchFromVagus) ;

           end ;

          { Update drugs in circulation }
          for i := 0 to High(Drugs) do if Drugs[i] <> Nil then
              begin
              Drugs[i].Dose := Drugs[i].Dose
                               + Drugs[i].OnRate*(Drugs[i].DoseInjected - Drugs[i].Dose)
                               - (Drugs[i].Dose*Drugs[i].RemovalRate) ;
              Drugs[i].Dose := Max( Drugs[i].Dose, 0. ) ;
              Drugs[i].DoseInjected := Max( Drugs[i].DoseInjected -
                                        Drugs[i].DoseInjected*Drugs[i].RemovalRate, 0. ) ;
              end ;


         { Muscarinic cholinoceptor activation }
         for i := 0 to High(Drugs) do if Drugs[i] <> Nil then
             MusChR := ReceptorActivation(i,Drugs[i].Dose,Drugs[i].MusChR,Num,Denom,1.0 ) ;

         { Vagally stimulated muscarinic cholinoceptor activation }
         for i := 0 to High(Drugs) do if Drugs[i] <> Nil then
             VagChR := ReceptorActivation(i,Drugs[i].Dose,Drugs[i].VagChR,Num,Denom,1.0 ) ;

         { Phosphodiesterase inhibition }
         for i := 0 to High(Drugs) do if Drugs[i] <> Nil then
             PDE := ReceptorActivation(i,Drugs[i].Dose,Drugs[i].PDE,Num,Denom,1.0 ) ;

         { Alpha1-adrenoceptor activation }
         for i := 0 to High(Drugs) do if Drugs[i] <> Nil then
             Alpha1AdR := ReceptorActivation(i,Drugs[i].Dose,Drugs[i].Alpha1AdR,Num,Denom,1.0 ) ;

         { Alpha2-adrenoceptor activation }
         for i := 0 to High(Drugs) do if Drugs[i] <> Nil then
             Alpha2AdR := ReceptorActivation(i,Drugs[i].Dose,Drugs[i].Alpha2AdR,Num,Denom,1.0 ) ;

         { Beta1-adrenoceptor activation }
         PotencyShift := 1.0 - PDE*0.5 ;
         for i := 0 to High(Drugs) do if Drugs[i] <> Nil then
             Beta1AdR := ReceptorActivation(i,Drugs[i].Dose,Drugs[i].Beta1AdR,Num,Denom,PotencyShift ) ;

        { Beta2-adrenoceptor activation }
        PotencyShift := 1.0 - PDE*0.5 ;
        for i := 0 to High(Drugs) do if Drugs[i] <> Nil then
            Beta2AdR := ReceptorActivation(i,Drugs[i].Dose,Drugs[i].Beta2AdR,
                        Num,Denom,PotencyShift ) ;

        { Heart muscle calcium channel activation }
        for i := 0 to High(Drugs) do if Drugs[i] <> Nil then
            HMCaChannels := ReceptorActivation(i,Drugs[i].Dose,Drugs[i].HMCaChannel,Num,Denom,1.0 ) ;

        { Digoxin effect on heart muscle }
        for i := 0 to High(Drugs) do if Drugs[i] <> Nil then
            DigR := ReceptorActivation(i,Drugs[i].Dose,Drugs[i].DigR,Num,Denom,1.0 ) ;

        { Smooth muscle calcium channel activation }
        for i := 0 to High(Drugs) do if Drugs[i] <> Nil then
            SMCaChannels := ReceptorActivation(i,Drugs[i].Dose,Drugs[i].SMCaChannel,Num,Denom,1.0 ) ;

        { Adenosine receptor activation }
        for i := 0 to High(Drugs) do if Drugs[i] <> Nil then
            AdenR := ReceptorActivation(i,Drugs[i].Dose,Drugs[i].AdenR,Num,Denom,1.0 ) ;

        { K channel activation }
        for i := 0 to High(Drugs) do if Drugs[i] <> Nil then
            KChannels := ReceptorActivation(i,Drugs[i].Dose,Drugs[i].KChannel,Num,Denom,1.0 ) ;

        { Nitric oxide synthase }
        for i := 0 to High(Drugs) do if Drugs[i] <> Nil then
            NOS := ReceptorActivation(i,Drugs[i].Dose,Drugs[i].NOS,Num,Denom,1.0 ) ;

        { Nitric oxide activation }
        { Note. Intrinsic generation of nitric oxide means that a certain
          degree of vasodilation exists in the absence of drugs }
        for i := 0 to High(Drugs) do if Drugs[i] <> Nil then
             NOX := ReceptorActivation(i,Drugs[i].Dose,Drugs[i].NOX,Num,Denom,1.0 ) ;

         { Angiotensin I activation }
        for i := 0 to High(Drugs) do if Drugs[i] <> Nil then
             Angt1R := ReceptorActivation(i,Drugs[i].Dose,Drugs[i].Angt1R,Num,Denom,1.0 ) ;

         { Angiotensin converting enzyme activity}
        for i := 0 to High(Drugs) do if Drugs[i] <> Nil then
             ACE := ReceptorActivation(i,Drugs[i].Dose,Drugs[i].ACE,Num,Denom,1.0 ) ;

         { Angiotensin II produce from Angiotensin I by angiotensin converting enzyme }
        Drugs[iAngioTensin2ACE].Dose := Drugs[iAngioTensin2ACE].Angt2R.Potency*5.0
                                         *( 1.0 - ACE )
                                         * (Angt1R / ( 1.0 + Angt1R )) ;

         { Angiotensin II activation }
        for i := 0 to High(Drugs) do if Drugs[i] <> Nil then
             Angt2R := ReceptorActivation(i,Drugs[i].Dose,Drugs[i].Angt2R,Num,Denom,1.0 ) ;

         { Bradykinin receptor activation }
        for i := 0 to High(Drugs) do if Drugs[i] <> Nil then
             BradR := ReceptorActivation(i,Drugs[i].Dose,Drugs[i].BradR,Num,Denom,1.0 ) ;

         t.next := t.time + 0.005 ;

        end ;


     Art.Conductance := 5.0*(1.0
                        + (2.5*MusChR*(1.0-NOS))  { Muscarinic receptors VD }
                        + ({2.25}1.9*Beta2AdR)   { Beta-2 adrenoceptor VD Decreased to 1.9 to allow alpha ADR vasosconstrictor to dominate at high concs.}
                        + (2.5*AdenR)             { Adenosine receptors VD }
                        + 2.0*NOX                 { Nitric oxide VD }
                        + 2.0*PDE                 { Phosphodiesterase inhibition VD }
                        + 0.95*SMCaChannels       { Ca channel block VD }
                        + 0.95*KChannels          { K channel opening VD }
                        + BradR                   { Bradykinin VD }
                        - 0.6*NOS                 { Inhib. of NO synthase VC }
                        - {0.9}1.35*Alpha1ADR     { Alpha-1 adrenoceptor VD Increased to 1.35 to emphasize vasoconstrictor effect at high concs. 22.07.18 }
                       { - 0.9*NicChR }            { Nicotinic VC }
                        - 0.9*Angt2R             { Angiotension II VC }

                        );
     Art.Conductance := Max( Art.Conductance,0.25 ) ;

     { Arterial elastance increased by vasoconstrictors
       Results in increased pulse pressure }
     Art.Elastance := (1./0.8) * (1.0
                      + 0.2*Alpha1Adr
                      );

     { Venous elastance : decreased by certain vasodilators
       Decreased elastance = increased venous capacity }
     Ven.Elastance := (1./30.0) * ( 1.0 - 0.7*Min( 1.0,
                      NOX
                      + SMCaCHannels
                      - Alpha1AdR
                      ) ) ;

     Heart.InitialisePressureRange ;
     Art.InitialisePressureRange ;
     Ven.InitialisePressureRange ;
     Con.InitialisePressureRange ;

     YMax := -1E38 ;
     YMin := 1E38 ;

     for i := 1 to NumStepsPerDisplayPoint do
         begin

         { Set heart rate and force for next beat}
         if t.Time >= Heart.NextBeat then
            begin

            Art.MeanPressure := (Art.MaxPressure - Art.MinPressure)*0.33
                                + Art.MinPressure ;
            Art.MinPressure := 300.0 ;
            Art.MaxPressure := 0.0 ;

            { Heart rate }
            Heart.Rate := 1. + 350.0*(1.0
                                        - 0.7*VagChR
                                        - MusChR
                                        - AdenR
                                        + Beta1AdR
                                        - HMCaChannels
                                        + 1.0*PDE ) ;
            Heart.Rate := Max( Heart.Rate, 1.0 ) ;

            { Contractile force }
            Heart.Force :=  8.0 * (1.0
                           - 0.95*MusChR
                           - 0.95*AdenR
                           - 0.6*KChannels
                           + 1.5*Beta1AdR
                           + DigR
                           + PDE )
                           * ( 1.0 - HMCaChannels )
                           * (1./(1. + Heart.Volume0/Heart.Volume)) ;

            { Duration of contraction (systole) }
            Heart.Time := (12.0 / 400.0) ;
            if Heart.Time >= (12.0/Heart.Rate) then Heart.Time := 12.0/Heart.Rate ;
            Heart.TSystole := Heart.Time ;

            { Time for next beat }
            Heart.NextBeat := t.Time + (60.0/Heart.Rate) ;

            end ;

         if Heart.Time > 0.0 then
           begin

           { Compute cardiac contractile force }
           TMid := t.step*Round((Heart.TSystole*0.5)/t.step) ;

           x := (Heart.Time-TMid)/(TMid*0.33) ;
       {    Heart.Elastance := (sin(3.141*(Heart.Time/Heart.TSystole))
                              * Heart.Force )
                              + Ven.Elastance*4.0 ;}

           Heart.Elastance := (exp(-x*x)
                              * Heart.Force )
                              + Ven.Elastance*4.0 ;

           Heart.Time := Heart.Time - t.Step ;
           end ;

         if DigR > 0.95 then
            begin
            Heart.Elastance := Heart.Force*AddNoise(0.5)*0.25 ;
            Fibrillation := Fibrillation + t.step ;
            Heart.Rate := 300.0*AddNoise(0.05) ;
            Heart.Pressure := 50.0*AddNoise(1.0 ) ;
            end
         else Heart.Pressure := Pressure(Heart) ;

         Con.Pressure := Heart.Elastance ;
         YMax := Max(Ymax,Heart.Elastance) ;
         YMin := Min(Ymin,Heart.Elastance) ;

         { Arterial valve }
         if (Heart.Pressure >= Art.Pressure) and (Fibrillation <= 0.0) then
            begin
            Heart.Flow := (Heart.Pressure - Art.Pressure)*Heart.Conductance ;
            Art.Volume := Art.Volume + Heart.Flow*t.step ;
            Heart.Volume := Heart.Volume - Heart.Flow*t.step ;
            end ;

         Art.Pressure := Pressure(Art) ;

         { Find Systolic/Diastolic pressures }
         Art.MaxPressure := Max(Art.MaxPressure,Art.Pressure) ;
         Art.MinPressure := Min(Art.MinPressure,Art.Pressure) ;

         Art.Flow := (Art.Pressure - Ven.Pressure)*Art.Conductance ;
         Ven.Volume := Ven.Volume + Art.Flow*t.step ;
         Art.Volume := Art.Volume - Art.Flow*t.step ;

         Ven.Pressure := Pressure(Ven) ;
         { Venous system -> heart valve }
         if (Ven.Pressure >= Heart.Pressure) and (Fibrillation <= 0.0)then
            begin
            Ven.Flow := (Ven.Pressure - Heart.Pressure)*Ven.Conductance ;
            Heart.Volume := Heart.Volume + Ven.Flow*t.step ;
            Ven.Volume := Ven.Volume - Ven.Flow*t.step ;
            end ;

         t.time := t.time + t.step ;

         Heart.CalculatePressureRange( t.time ) ;
         Art.CalculatePressureRange( t.time ) ;
         Ven.CalculatePressureRange( t.time ) ;
         Con.CalculatePressureRange( t.time ) ;

         end ;

     // First point
     ChanValues[chLVP] := Heart.GetLeadingPressure ;

     ChanValues[chABP] := Art.GetLeadingPressure ;
     ChanValues[ChCVP] := Ven.GetLeadingPressure ;
     ChanValues[ChHCF] := Con.GetLeadingPressure ;
     ChanValues[chHR] := Heart.Rate*AddNoise(0.00) ;

     // Next point
     ChanValues[NumChannels+chLVP] := Heart.GetTrailingPressure ;
     ChanValues[NumChannels+chABP] := Art.GetTrailingPressure ;
     ChanValues[NumChannels+ChCVP] := Ven.GetTrailingPressure ;
     ChanValues[NumChannels+ChHCF] := Con.GetTrailingPressure ;
     ChanValues[NumChannels+chHR] := Heart.Rate*AddNoise(0.00) ;

  //   Log.d(format('%.4g %.4g %.4g %.4g %.4g',[t.time,Alpha1Adr,Alpha2Adr,Beta1Adr,Beta2Adr]));

     // Determine if animal is dead (due to low blood pressure)
     if not Dead then
        begin
        if Art.Pressure < 20. then Inc(Dying)
                              else Dying := 0 ;
        if Dying = 500 then Dead := True ;
        end ;

     end ;


procedure TElement.CalculatePressureRange( t : single ) ;
begin
     if Pressure <= PressureLo then
        begin
        PressureLo := Pressure ;
        TLo := t ;
        end ;

     if Pressure >= PressureHi then
        begin
        PressureHi := Pressure ;
        THi := t ;
        end ;
     end ;


function TElement.GetLeadingPressure : single ;
begin
     if TLo <= THi then Result := PressureLo
                   else Result := PressureHi ;
     end ;

function TElement.GetTrailingPressure : single ;
begin
     if THi >= TLo then Result := PressureHi
                   else Result := PressureLo ;
     end ;

procedure TElement.InitialisePressureRange ;
begin
     PressureHi := -1E38 ;
     PressureLo := 1E38 ;
     end ;


constructor TDrug.Create(
          NameIn : String ;
          ShortNameIn : string
          ) ;
// ------------------
// Create drug object
// ------------------
begin
     Inherited Create   ;
     Name := NameIn ;
     ShortName := ShortNameIn ;
     Dose := 0.0 ;
     DoseInjected := 0.0 ;
     Conc := 0.0 ;
     Alpha1AdR.Efficacy := None ;
     Alpha1AdR.Potency := None ;
     Alpha2AdR.Efficacy := None ;
     Alpha2AdR.Potency := None ;
     Beta1AdR.Efficacy := None ;
     Beta1AdR.Potency := None ;
     Beta2AdR.Efficacy := None ;
     Beta2AdR.Potency := None ;
     MusChR.Efficacy := None ;
     MusChR.Potency := None ;
     VagChR.Efficacy := None ;
     VagChR.Potency := None ;
     NicChR.Efficacy := None ;
     NicChR.Potency := None ;
     HMCaChannel.Efficacy := None ;
     HMCaChannel.Potency := None ;
     SMCaChannel.Efficacy := None ;
     SMCaChannel.Potency := None ;
     AdenR.Efficacy := None ;
     AdenR.Potency := None ;
     KChannel.Efficacy := None ;
     KChannel.Potency := None ;
     NOX.Efficacy := None ;
     NOX.Potency := None ;
     NOS.Efficacy := None ;
     NOS.Potency := None ;
     Angt1R.Efficacy := None ;
     Angt1R.Potency := None ;
     Angt2R.Efficacy := None ;
     Angt2R.Potency := None ;
     BradR.Efficacy := None ;
     BradR.Potency := None ;
     ACE.Efficacy := None ;
     ACE.Potency := None ;
     DigR.Efficacy := None ;
     DigR.Potency := None ;
     PDE.Efficacy := None ;
     PDE.Potency := None ;
     OnRate := 1.0 ;
     RemovalRate := 0.05 ;
     end ;




procedure TModel.SetAgonistPotency(
          var Drug : TDrugProperties ;
          Potency : single
          ) ;
begin
      if Potency <> None then
        begin
        Drug.Potency := Potency*AddNoise(0.0) ;
        { Efficacy - 1.0=agonist, 0.0=antagonist }
        Drug.Efficacy := 1.0 ;
        end
     else Drug.Efficacy := None ;
     end ;


function TModel.ReceptorActivation(
          iDrug : Integer ;
          Dose : single ;
          var Drug : TDrugProperties ;
          var Numerator : single ;
          var Denominator : single ;
          PotencyShift : single
          ) : single ;
var
   x : single ;
begin

     if iDrug = 0 then
        begin
        Numerator := 0.0 ;
        Denominator := 1.0 ;
        end ;
     if Drug.Efficacy <> None then
        begin
        x := Dose/(Drug.Potency*PotencyShift) ;
        Denominator := Denominator + x ;
        Numerator := Numerator + Drug.Efficacy*x ;
        end ;
     if Denominator > 0. then Result := Numerator/Denominator
                         else Result := 0. ;

     end ;


procedure TModel.SetAntagonistPotency(
          var Drug : TDrugProperties ;
          Potency : single
          ) ;
begin
      if Potency <> None then
        begin
        Drug.Potency := Potency*AddNoise(0.0) ;
        { Efficacy - 1.0=agonist, 0.0=antagonist }
        Drug.Efficacy := 0.0 ;
        end
     else Drug.Efficacy := None ;
     end ;


function TModel.Pressure( Element : TElement ) : single ;
var
   P : Single ;
begin
     P := (Element.Volume - Element.Volume0)*Element.Elastance ;
     if P < 0.0 then P := 0.0 ;
     Result := P ;
     end ;


function TModel.AddNoise(
         Proportion : single
         ) : single ;
{ -----------------------------------------------
  Create a random scaling factor 1 +/- Proportion
  -----------------------------------------------}
begin
     AddNoise := 1. + ((2.*random)-1. )*Proportion ;
     end ;




function TModel.ExtractFloat ( CBuf : string ; Default : Single ) : extended ;
{ Extract a floating point number from a string which
  may contain additional non-numeric text }

var CNum : string ;
i : SmallInt ;

begin
     CNum := ' ' ;
     for i := 1 to length(CBuf) do begin
         if CharInSet( CBuf[i], ['0'..'9', 'E', 'e', '+', '-', '.', ',' ] ) then
            CNum := CNum + CBuf[i]
         else CNum := CNum + ' ' ;
         end ;

     { Correct for use of comma/period as decimal separator }
     if (formatsettings.DECIMALSEPARATOR = '.') and (Pos(',',CNum) <> 0) then
        CNum[Pos(',',CNum)] := formatsettings.DECIMALSEPARATOR ;
     if (formatsettings.DECIMALSEPARATOR = ',') and (Pos('.',CNum) <> 0) then
        CNum[Pos('.',CNum)] := formatsettings.DECIMALSEPARATOR ;

     try
        ExtractFloat := StrToFloat( CNum ) ;
     except
        on E : EConvertError do ExtractFloat := Default ;
        end ;
     end ;





end.
