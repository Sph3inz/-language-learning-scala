package models

case class User(
  id: String,
  username: String,
  email: String,
  passwordHash: String,
  createdAt: Long = System.currentTimeMillis()
)

case class UserCredentials(
  email: String,
  password: String
)

case class SignUpRequest(
  username: String,
  email: String,
  password: String
)

case class GuestUserRequest(
  username: String
) 