people <- data.frame(name=c('Maria','Rachel','Alexander'),
                     surname=c('Ivanova','Petrova','Sidorov'),
                     age=c(22,27,25),
                     gender=c('F','F','M'))
rownames(people) <- people$surname
rownames(people)
people['Ivanova',]
people[which(people$gender == 'M'),]
people$name
people[2,]


nums <- runif(100,0,7)
nums[sin(nums) > 0]
nums2 <- matrix(nums,nrow=10)
nums2[nums2[,6]>4,unique(which(nums2<1,arr.ind=T)[,2])]


      
