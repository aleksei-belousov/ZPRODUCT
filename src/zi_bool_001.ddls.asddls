@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZI_BOOL_001'
define root view entity ZI_BOOL_001 as select from zbool001
{
    key uuid as UUID,
    id as ID,
    text as Text
}
