Products Data Export

This portion of your data export includes data on your Premium subscriptions

User Premium Subscriptions.csv:
---------
      provider_id         - subscription provider, apple or google
      sku                 - Fitbit name of premium product
      product_description - premium product short description
      product_reccurence  - premium product recurrence
      valid_until         - the date premium subscription is valid until
      receipt             - encoded Apple or Google receipt with your subscriptions data
      currency            - currency used to purchase premium subscription (might be empty)
      fatal_renewal_error - if 'TRUE' subscription renewal will not be retried
      grant_bundle        - the name of features set included into the subscription
      portal_url          - should be empty

User Premium Transactions.csv:
---------
      provider_id             - subscription provider, apple or google
      sku                     - Fitbit name of premium product
      timestamp               - date and time of the transaction
      provider_transaction_id - transaction identifier received from Apple or Google