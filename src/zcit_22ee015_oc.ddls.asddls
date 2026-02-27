@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order Item Consumption View'
@Search.searchable: true
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZCIT_22EE015_OC
  as projection on ZCIT_22EE015_O
{
  key SalesDocument,
  key SalesItemnumber,
      @Search.defaultSearchElement: true
      Material,
      @Semantics.quantity.unitOfMeasure: 'Quantityunits'
      Quantity,
      Quantityunits,
      Plant,
      LocalCreatedBy,
      LocalCreatedAt,
      LocalLastChangedBy,
      LocalLastChangedAt,
      
      /* Associations */
      _salesHeader : redirected to parent ZCIT_22EE015_C
}
