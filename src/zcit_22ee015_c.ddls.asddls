@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order Header Consumption View'
@Search.searchable: true
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZCIT_22EE015_C
  provider contract transactional_query
  as projection on ZCIT_22EE015_I
{
  key SalesDocument,
      SalesDocumentType,
      OrderReason,
      SalesOrganization,
      DistributionChannel,
      Division,
      @Search.defaultSearchElement: true
      SalesOffice,
      SalesGroup,
      @Semantics.amount.currencyCode: 'Currency'
      NetPrice,
      Currency,
      LocalCreatedBy,
      LocalCreatedAt,
      LocalLastChangedBy,
      LocalLastChangedAt,
      
      /* Associations */
      _salesitem : redirected to composition child ZCIT_22EE015_OC
}
