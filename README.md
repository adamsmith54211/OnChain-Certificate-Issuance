# OnChain Certificate Smart Contract

A Clarity smart contract for issuing immutable academic certificates as NFTs on the Stacks blockchain.

## Features

- Issue academic certificates as non-transferable NFTs
- Store certificate metadata on-chain
- Authorized institutions can issue certificates
- Students can view their certificates
- Certificate data includes course, grade, institution, and timestamp

## Usage

### For Institutions

1. Get authorized by contract owner
2. Issue certificates using `issue-certificate`

```clarity
(contract-call? .onchain-cert issue-certificate 
    'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM 
    "Computer Science 101" 
    "A+" 
    "Stanford University")
```

### For Students

View your certificates using `get-certificates-by-recipient`

```clarity
(contract-call? .onchain-cert get-certificates-by-recipient tx-sender)
```

### For Contract Owner

Manage authorized issuers:

```clarity
(contract-call? .onchain-cert add-authorized-issuer 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

## Security

- Certificates are non-transferable
- Only authorized institutions can issue certificates
- Certificate data is immutable once issued
```
