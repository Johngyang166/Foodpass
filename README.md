# Foodpass - Tokenized Food Ration Cards

A Clarity smart contract implementation for QR-based digital food ration cards on the Stacks blockchain. Provides secure, transparent, and efficient distribution of monthly food rations to beneficiaries.

## Features

- **Tokenized Ration System**: Digital food tokens (foodpass-token) representing monthly rations
- **QR Code Authentication**: Secure QR code-based card verification and claiming
- **Monthly Distribution**: Automated monthly ration allocation based on family size
- **Card Management**: Issue, activate, deactivate, and extend ration cards
- **Emergency Distribution**: Authorized distributors can provide emergency rations
- **Transfer Capability**: Beneficiaries can transfer rations to others
- **Family Size Adjustment**: Update family size and corresponding ration amounts

## Contract Functions

### Administrative Functions

#### `issue-ration-card`
```clarity
(issue-ration-card beneficiary family-size validity-months)
```
Issues a new ration card to a beneficiary.
- `beneficiary`: Principal receiving the card
- `family-size`: Number of family members (1-10)
- `validity-months`: Card validity period in months

#### `deactivate-card` / `reactivate-card`
```clarity
(deactivate-card card-id)
(reactivate-card card-id)
```
Deactivate or reactivate a ration card.

#### `authorize-distributor` / `revoke-distributor`
```clarity
(authorize-distributor distributor)
(revoke-distributor distributor)
```
Manage authorized distributors for emergency rations.

### Beneficiary Functions

#### `claim-monthly-ration`
```clarity
(claim-monthly-ration card-id qr-code-hash)
```
Claim monthly rations using card ID and QR code hash.

#### `transfer-rations`
```clarity
(transfer-rations recipient amount)
```
Transfer ration tokens to another principal.

### Read-Only Functions

#### `get-card-details`
```clarity
(get-card-details card-id)
```
Returns complete card information including beneficiary, family size, allocation, and status.

#### `get-token-balance`
```clarity
(get-token-balance user)
```
Returns current ration token balance for a user.

#### `is-card-valid`
```clarity
(is-card-valid card-id)
```
Checks if a card is active and not expired.

## Usage Instructions

### 1. Deploy Contract
Deploy the contract to Stacks blockchain using Clarinet.

### 2. Issue Ration Cards
Contract owner issues cards to beneficiaries:
```clarity
(contract-call? .foodpass issue-ration-card 'SP1BENEFICIARY123 u4 u12)
```

### 3. Claim Monthly Rations
Beneficiaries claim their monthly allocation:
```clarity
(contract-call? .foodpass claim-monthly-ration u1 0x1234abcd...)
```

### 4. Transfer Rations
Transfer tokens between users:
```clarity
(contract-call? .foodpass transfer-rations 'SP1RECIPIENT123 u500000)
```

### 5. Emergency Distribution
Authorized distributors can distribute emergency rations:
```clarity
(contract-call? .foodpass distribute-emergency-rations (list 'SP1USER1 'SP1USER2) u200000)
```

## Constants

- `MONTHLY_RATION_AMOUNT`: 1,000,000 tokens per family member
- `BLOCKS_PER_MONTH`: 4,320 blocks (approximately 30 days)
- `MAX_FAMILY_SIZE`: 10 members maximum
- `TOKEN_DECIMALS`: 6 decimal places

## Security Features

- QR code hash verification for secure claiming
- Monthly claim limits to prevent abuse
- Card expiration and activation controls
- Authorized distributor system for emergency relief
- Principal validation to prevent unauthorized access

## Development

### Prerequisites
- Clarinet CLI
- Node.js (for testing)

### Testing
```bash
clarinet test
```

### Local Development
```bash
clarinet console
```

### Deploy
```bash
clarinet deploy
```

## Error Codes

- `u100`: Unauthorized access
- `u101`: Invalid amount
- `u102`: Insufficient balance
- `u103`: Card not found
- `u104`: Card expired
- `u105`: Card already exists
- `u106`: Monthly limit exceeded
- `u107`: Invalid QR code
- `u108`: Card inactive
- `u109`: Invalid beneficiary
- `u110`: Distribution failed
