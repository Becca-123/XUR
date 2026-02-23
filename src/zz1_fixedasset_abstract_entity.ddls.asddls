@EndUserText.label: 'ABSTRACT_ENTITY FOR 固定資產主檔大量修改的API'
define root abstract entity ZZ1_FIXEDASSET_ABSTRACT_ENTITY
{
    @UI.defaultValue : #( 'ELEMENT_OF_REFERENCED_ENTITY: CostCenter')
    @Consumption.valueHelpDefinition: [{  entity: {name: 'I_CostCenterStdVH', element: 'CostCenter'} }]     
    @EndUserText.label: '成本中心'
    CostCenter : abap.char(10);
    
    @UI.defaultValue : #( 'ELEMENT_OF_REFERENCED_ENTITY: Plant')
    @Consumption.valueHelpDefinition: [{  entity: {name: 'I_PlantStdVH', element: 'Plant'} }]     
    @EndUserText.label: '工廠'
    Plant : abap.char(4);
    
//    @UI.defaultValue : #( 'ELEMENT_OF_REFERENCED_ENTITY: wbselementexternalid')
//    @EndUserText.label: 'WBS ID'   
//    @Consumption.valueHelpDefinition: [{  entity: {name: 'ZZ1_I_wbselement_VH', element: 'wbselementexternalid'},                                                                                          
//               additionalBinding: [{  element: 'ActivityType' , localElement: 'ActivityType', usage:  #FILTER_AND_RESULT },
//               {  element: 'billingcontrolcategoryid' , localElement: 'billingcontrolcategoryid', usage:  #FILTER_AND_RESULT }]
//               }] 
//    @UI: { lineItem:       [ { position:140, importance: #HIGH } ],
//           // selectionField: [{ position: 90 }],
//          identification: [ { position: 140 } ] }        
//    wbselementexternalid : abap.char(24);

    @UI.defaultValue : #( 'ELEMENT_OF_REFERENCED_ENTITY: AssetLocation')
    @Consumption.valueHelpDefinition: [{  entity: {name: 'ZZ1_I_LOCATION', element: 'Location'} }]       
    @EndUserText.label: '地點'
    @UI: { lineItem:       [ { position:150, importance: #HIGH } ],
          identification: [ { position: 150 } ] }
    AssetLocation : abap.char(10);
    
    @UI.defaultValue : #( 'ELEMENT_OF_REFERENCED_ENTITY: Room')
    @EndUserText.label: '室'
    @UI: { lineItem:       [ { position:160, importance: #HIGH } ],
          identification: [ { position: 160 } ] }
    Room : abap.char(8);
    
    @UI.defaultValue : #( 'ELEMENT_OF_REFERENCED_ENTITY: CalendarDate') //#( 'ELEMENT_OF_REFERENCED_ENTITY: CalendarDate') //#( 'ELEMENT_OF_REFERENCED_ENTITY: ValidityStartDate')
    @EndUserText.label: '生效日期'
    @UI: { lineItem:       [ { position:170, importance: #HIGH } ],
          identification: [ { position: 170 } ] }
    ValidityStartDate : abap.dats ;
}
