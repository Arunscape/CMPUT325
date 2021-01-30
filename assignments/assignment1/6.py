#L = (('google', 'shopify'), ('google', 'aircanada'), ('amazon', 'aircanada'))
#L = ( ('google', 'shopify'), ('shopify', 'amazon'), ('amazon', 'google') )
#L = ( ('google', 'shopify'), ('shopify', 'amazon'), ('amazon', 'indigo')  )
#L = ( ('google', 'shopify'), ('google', 'aircanada'), ('amazon', 'aircanada'), ('aircanada', 'delta'), ('google', 'google') )

L = ( ('shopify', 'aircanada'), ('google', 'shopify')) 

def imparative_soln():
    global L

    acc = dict()
    
    for (k,v) in L:
        acc[k] = v
    
    x = acc['google']
    
    while x is not None:
        print(x)
        x = acc.get(x)
    


imparative_soln()
print()


def car(l):
    return l[0]

def cdr(l):
    return l[1:]

#weird stuff
def cadr(l):
    return car(cdr(l))

def equal(x,y):
    return x==y

def reached(x, L):
    return reached_helper(x,L,[])

def b_if_x_is_a_else_nil(x, P):
    if equal(x, car(P)):
        return cadr(P)
    return None

def reached_helper(x,L,acc):
   
    # base case if none of the things are reachable
    if all(map(lambda h: not equal(x, car(h)), L)):
        return acc

    # make a copy so I'm sure that I'm actually doing this functionally lol
    new_acc = acc
    for l in L:
        if equal(car(l),x):
            new_acc.append(cadr(l))
            return reached_helper(cadr(l), L, new_acc)

x = reached('google', L)
print(x)
