@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '儲存地點 Value Help'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZZ1_I_LOCATION as select from I_Location
{
    
     @ObjectModel.text.element: ['LocationName']
      @Search: {defaultSearchElement: true, ranking: #HIGH, fuzzinessThreshold: 0.8}
  key Location,
      @ObjectModel.foreignKey.association: '_Plant'
  key Plant,
      AddressID,
      @Search: {defaultSearchElement: true, ranking: #LOW, fuzzinessThreshold: 0.8}
      @Semantics.text: true
      LocationName,

      // Associations
      _Plant,
      _Address,
      _Address_2
}
