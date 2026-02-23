@AccessControl.authorizationCheck: #NOT_REQUIRED
@ObjectModel.resultSet.sizeCategory: #XS
@ObjectModel.supportedCapabilities: [ #ANALYTICAL_DIMENSION, #CDS_MODELING_ASSOCIATION_TARGET, #SQL_DATA_SOURCE, #CDS_MODELING_DATA_SOURCE ]
@EndUserText.label: '固定資產主檔大量修改的API'
define root view entity ZZ1_I_FIXEDASSET 
    as select from I_FixedAsset as _FixedAsset
        inner join ZZ1_I_FixedAssetAssgmt_MAX as _max on _FixedAsset.CompanyCode = _max.CompanyCode and _FixedAsset.MasterFixedAsset = _max.MasterFixedAsset 
                                                       and _FixedAsset.FixedAsset = _max.FixedAsset
        left outer join I_FixedAssetAssgmt as _FixedAssetAssgmt on _max.CompanyCode = _FixedAssetAssgmt.CompanyCode and _max.MasterFixedAsset = _FixedAssetAssgmt.MasterFixedAsset 
                                                               and _max.FixedAsset = _FixedAssetAssgmt.FixedAsset and _max.ValidityStartDate = _FixedAssetAssgmt.ValidityStartDate
        left outer join I_CalendarDate as _CalendarDate on _CalendarDate.CalendarDate = $session.system_date
{
    key _FixedAsset.CompanyCode,
    key _FixedAsset.MasterFixedAsset, 
    key _FixedAsset.FixedAsset, 
    key _max.ValidityStartDate,
        _FixedAssetAssgmt.CostCenter, 
        _FixedAssetAssgmt.ResponsibleCostCenter, 
        _FixedAssetAssgmt.Plant,
        _FixedAssetAssgmt.AssetLocation, 
        _FixedAssetAssgmt.Room, 
        _FixedAssetAssgmt.InternalOrder, 
        _FixedAssetAssgmt.PersonnelNumber,
        _FixedAssetAssgmt.FunctionalArea,
        _FixedAssetAssgmt.IsShutDown,
        _CalendarDate.CalendarDate,
        
//        cast( '' as abap.char( 1 ) ) as error,
//        cast( '' as abap.char( 220 ) ) as message,
        cast( '' as abap.char( 1 ) ) as updatedata
        
} where _FixedAsset.AssetDeactivationDate is initial and ( _FixedAsset.AssetLifecycleStatus = '1' or _FixedAsset.AssetLifecycleStatus = '4' )
  group by _FixedAsset.CompanyCode, _FixedAsset.MasterFixedAsset, _FixedAsset.FixedAsset, _max.ValidityStartDate,
           _FixedAssetAssgmt.CostCenter, _FixedAssetAssgmt.ResponsibleCostCenter, _FixedAssetAssgmt.Plant, _FixedAssetAssgmt.AssetLocation, 
           _FixedAssetAssgmt.Room,  _FixedAssetAssgmt.InternalOrder, _FixedAssetAssgmt.PersonnelNumber, _FixedAssetAssgmt.FunctionalArea, 
           _FixedAssetAssgmt.IsShutDown, _CalendarDate.CalendarDate
