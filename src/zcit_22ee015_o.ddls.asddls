@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Child Interface View for the Items'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZCIT_22EE015_O
  as select from zmk_item_t
  association to parent ZCIT_22EE015_I as _salesHeader 
    on $projection.SalesDocument = _salesHeader.SalesDocument
{
  key salesdocument     as SalesDocument,
  key salesitemnumber   as SalesItemnumber,
      material          as Material,
      plant             as Plant,
      @Semantics.quantity.unitOfMeasure: 'Quantityunits'
      quantity          as Quantity,
      quantityunits     as Quantityunits,
      @Semantics.user.createdBy: true
      local_created_by  as LocalCreatedBy,
      @Semantics.systemDateTime.createdAt: true
      local_created_at  as LocalCreatedAt,
      @Semantics.user.lastChangedBy: true
      local_last_changed_by as LocalLastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at   as LastChangedAt,

      /* Associations */
      _salesHeader
}
