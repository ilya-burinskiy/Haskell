usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = asks $ map fst . filter (\r -> snd r == "123456")
