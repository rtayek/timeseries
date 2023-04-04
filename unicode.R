# https://stackoverflow.com/questions/71587307/how-to-generate-all-possible-unicode-characters
varname <- c("a", "b", "c")
labels <- c("A \U00B5 g/dL", "B \U00B5 g/dL", "C \U00B5 g/dL")
df <- data.frame(varname, labels)
test <- data.frame(varname, labels)
test
print(labels)
sessionInfo()
cat("Stra\U{00DF}e", "\n")
μ <- 2
str(μ )
print(μ)
x<-" äöüßâçèéêîôûąęćśłńóżźžšůěřáúëïùÄÖÜSSÂÇÈÉÊÎÔÛĄĘĆŚŁŃÓŻŹŽŠŮĚŘÁÚËÏÙ"
print(x)
äöüßâçèéêîôûąęćśłńóżźžšůěřáúëïùÄÖÜSSÂÇÈÉÊÎÔÛĄĘĆŚŁŃÓŻŹŽŠŮĚŘÁÚËÏÙ=42
y<- äöüßâçèéêîôûąęćśłńóżźžšůěřáúëïùÄÖÜSSÂÇÈÉÊÎÔÛĄĘĆŚŁŃÓŻŹŽŠŮĚŘÁÚËÏÙ+1
print(y)


