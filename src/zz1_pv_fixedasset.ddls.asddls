@EndUserText.label: '固定資產主檔大量修改的API Projection View'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Search.searchable: true
@Metadata.ignorePropagatedAnnotations: true
@UI: {
     headerInfo: {
                typeName: '固定資產主檔大量修改',
                typeNamePlural: '固定資產主檔大量修改'
     }
}
define root view entity ZZ1_PV_FIXEDASSET
  as projection on ZZ1_I_FIXEDASSET
{       
    
    @UI.facet: [ 
              {   
                  id:  'ZZ1_I_TIME_RECORD',
                  purpose:         #STANDARD,
                  type:            #IDENTIFICATION_REFERENCE,
                  label:           'DATA',
                  position:        10 
              }      
           ]
    
    
    @UI: { 
           lineItem: [{ position:10, importance: #HIGH }],
           selectionField: [{ position:10 }] }
    @Search.defaultSearchElement: true
    @EndUserText.label: '公司代碼'
    key CompanyCode,
    
    @UI: { lineItem: [{ position:20, importance: #HIGH }],
           selectionField: [{ position:20 }] }
    @EndUserText.label: '資產'
    key MasterFixedAsset, 
    
    @UI: { lineItem: [{ position:30, importance: #HIGH }],
           selectionField: [{ position:30 }] }
    @EndUserText.label: '資產子編號'
    key FixedAsset, 
    
    @UI: { lineItem: [{ position:35, importance: #HIGH }],
           selectionField: [{ position:35 }] }
    @EndUserText.label: '生效日期'
    key ValidityStartDate,
    
    @UI: { lineItem: [{ position:40, importance: #HIGH }],
           selectionField: [{ position:40 }] }
    @Consumption.valueHelpDefinition: [{  entity: {name: 'I_CostCenterStdVH', element: 'CostCenter'} }]     
    @EndUserText.label: '成本中心'
    CostCenter, 
    
    @UI: { lineItem: [{ position:50, importance: #HIGH }],
           selectionField: [{ position:50 }] }
    @EndUserText.label: '負責成本中心'
    ResponsibleCostCenter, 
    
    @UI: { lineItem: [{ position:60, importance: #HIGH }],
           selectionField: [{ position:60 }] }
    @Consumption.valueHelpDefinition: [{  entity: {name: 'I_PlantStdVH', element: 'Plant'} }]      
    @EndUserText.label: '工廠'
    Plant,
    
    @UI: { lineItem: [{ position:70, importance: #HIGH }],
           selectionField: [{ position:70 }] }
    @Consumption.valueHelpDefinition: [{  entity: {name: 'ZZ1_I_LOCATION', element: 'Location'} }]    
    @EndUserText.label: '地點'
    AssetLocation, 
    
    @UI: { lineItem: [{ position:80, importance: #HIGH }],
           selectionField: [{ position:80 }] }
    @EndUserText.label: '室'
    Room, 

    @UI: { lineItem: [{ position:90, importance: #HIGH }],
           selectionField: [{ position:90 }] }
    @EndUserText.label: '內部訂單'
    InternalOrder, 
    
    @UI: { lineItem: [{ position:100, importance: #HIGH }],
           selectionField: [{ position:100 }] }
    @EndUserText.label: '員工號碼'
    PersonnelNumber,
    
    @UI: { lineItem: [{ position:110, importance: #HIGH }],
           selectionField: [{ position:110 }] }
    @EndUserText.label: '功能範圍'
    FunctionalArea,
    
//    @UI.hidden: true
//    @Consumption.filter.hidden: true
    @UI: { lineItem: [{ position:120, importance: #HIGH }],
           selectionField: [{ position:120 }] }
    @EndUserText.label: '資產停置'
    IsShutDown,
    
//    @UI: { lineItem: [{ position:130, importance: #HIGH }] }
//    @EndUserText.label: '錯誤'
//    error,
//    
//    @UI: { lineItem: [{ position:150, importance: #HIGH }] }
//    @EndUserText.label: '錯誤訊息'
//    message,
    
    @UI.hidden: true
    CalendarDate,
    @UI: { lineItem: [{ 
            type : #FOR_ACTION,
            dataAction : 'UpdateData',
            label: 'Update',
            position:999, importance: #HIGH }]}
//    @UI.hidden: true
//    @Consumption.filter.hidden: true
    updatedata
}
