# [lens pun goes here]

![](../Lens1.png)

an exploration of <strike>strong</strike> <strike>gradual</strike>  _appropriate_ typing



# Foci

* What/why are lenses?
* Lenses and type.
* Language wars!
* Intro to ```core.typed```.
* Lenses with core.typed.
* Var Laarhoven lenses
* What is best?



# What are lenses?

* Essentially: a tool for convenient access to fields of nested structures, especially immutable ones.
* Originally: something fancy about bidirectional programming.
* Standard example:
~~~.scala
    case class Point(x: Double, y: Double)
    case class Color(r: Byte, g: Byte, b: Byte)
    case class Turtle(position: Point, heading: Double, color: Color)
~~~
* Standard example:
~~~.clj
    (defrecord Point [^double x ^double y])
    (defrecord Color [^short r ^short g ^short b])
    (defrecord Turtle [^Point position ^double heading ^Color color])
    (def t (->Turtle (->Point 1.0 2.0) (/ Math/PI 4) (->Color 255 0 0)))
~~~
* So what, we have ```assoc-in``` and ```get-in```?



# AWS

Java is the "Kingdom of Nouns"

~~~.java
RequestSpotInstancesRequest
   requestSpotInstancesRequest = new RequestSpotInstancesRequest();
LaunchSpecification
   launchSpecification = new LaunchSpecification();
InstanceNetworkInterfaceSpecification
   networkInterface = new InstanceNetworkInterfaceSpecification();
List<InstanceNetworkInterfaceSpecification>
   networkInterfaces = new ArrayList<InstanceNetworkInterfaceSpecification>();

networkInterface.setSubNetId("subnet-whatever")
netWorkInterfaces.add(networkInterface);
launchSpecification.setNetworkInterfaces(networkInterfaces);
requestSpotInstancesRequest.setLaunchSpecification(launchSpecification);

RequestSpotInstancesResult spotInstancesResult = requestSpotInstances(requestSpotInstanceRequests);
~~~



# AWS / amazonica

* Much nicer:
~~~.clj
{:spot-price 0.01, 
   :instance-count 1, 
   :type "one-time", 
   :launch-specification
   {:image-id "ami-something",
    :instance-type "t1.micro",
    :placement  {:availability-zone "us-east-1a"},
    :key-name "your-key"
    :user-data "WWFua2VlIGRvb2RsZSB3ZW50IHRvIHRvd24gcmlkaW5nIG9uIGEgcG9ueQo="
    :network-interfaces
    [{:device-index 0
      :subnet-id "subnet-yowsa"
      :groups ["sg-hubba"]}]
    :iam-instance-profile
    {:arn "arn:aws:iam::123456789:instance-profile/name-you-chose"}}}
~~~	




# But...

~~~.clj
(assoc-in my-req [:launch-specification 0 :subnet-id] "subnet-yowsa")
~~~
* Maybe not so good for separation of concerns.
* Easy to make mistakes.
* ```WWFua2VlIGRvb2RsZSB3ZW50IHRvIHRvd24gcmlkaW5nIG9uIGEgcG9ueQo=```?



# Clojure lenses with type

But first...



# Introduction to core.typed

* Optional/Gradual typing:
 * "Annotate" definitions with ```(t/ann my-function ...)```
 * Type-check namespace with ```(t/check-ns)```
 * Examine and check types in REPL with ```(t/cf)```
* More than validation sugar:
 * Type inference
 * Occurence typing
 * Polymorphism



# Code


