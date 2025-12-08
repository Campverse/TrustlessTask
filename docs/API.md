# API Documentation

## Base URL
```
http://localhost:8080/api/v1
```

## Endpoints

### Projects

#### Create Project
```http
POST /projects
Content-Type: application/json

{
  "title": "Website Development",
  "description": "Build a responsive website",
  "clientAddress": "addr_test1...",
  "freelancerAddress": "addr_test1...",
  "totalAmount": 1000000000,
  "milestones": [
    {
      "description": "Design mockups",
      "amount": 300000000,
      "deadline": "2024-12-31T23:59:59Z"
    }
  ],
  "arbiterAddress": "addr_test1..."
}
```

#### List Projects
```http
GET /projects
```

#### Get Project
```http
GET /projects/:projectId
```

### Milestones

#### Complete Milestone
```http
POST /projects/:projectId/milestone/:milestoneId/complete
```

#### Approve Milestone
```http
POST /projects/:projectId/milestone/:milestoneId/approve
```

### Users

#### Get Profile
```http
GET /users/:address/profile
```

### Disputes

#### Create Dispute
```http
POST /disputes
Content-Type: application/json

{
  "projectId": "uuid",
  "reason": "Work not delivered",
  "raiserAddress": "addr_test1..."
}
```
