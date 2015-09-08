user_data = read.csv('input/user_list.csv',
                     colClasses = c('character',
					                'factor',
									'integer',
									'character',
									'character',
									'character'))

user_data <- user_data[1:10, ]
user_data

str(user_data)
summary(user_data)

