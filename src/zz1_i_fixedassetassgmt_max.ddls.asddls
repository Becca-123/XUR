@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '固定資產時間相關資料 有效日期最新的資料'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZZ1_I_FixedAssetAssgmt_MAX as select from I_FixedAssetAssgmt as _FixedAssetAssgmt
{
    
    key _FixedAssetAssgmt.CompanyCode,
    key _FixedAssetAssgmt.MasterFixedAsset, 
    key _FixedAssetAssgmt.FixedAsset, 
        max(_FixedAssetAssgmt.ValidityStartDate) as ValidityStartDate
} group by _FixedAssetAssgmt.CompanyCode, _FixedAssetAssgmt.MasterFixedAsset,  _FixedAssetAssgmt.FixedAsset
