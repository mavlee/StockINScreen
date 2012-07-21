package controllers

import io.Source
import java.net.URL
import java.io.FileNotFoundException
import play.api._
import play.api.mvc._
import org.scribe.oauth.OAuthService
import org.scribe.builder.ServiceBuilder
import org.scribe.builder.api.LinkedInApi
import org.scribe.builder.api.Api
import org.scribe.model.Verifier
import org.scribe.model.Token
import org.scribe.model.Response
import org.scribe.model.OAuthRequest
import org.scribe.model.Verb
import com.google.gson.Gson
import json.Profile
import json.Connections
import json.Company
import json.Positions
import scala.actors.Future
import scala.actors.Futures._

object Application extends Controller {

  //get the auth token out and get data otherwise redirect them
  def index = Action { implicit request =>
    val accToken = session.get("acc_token")
    val accSecret = session.get("acc_secret")
    accToken match {
      case Some(k:String) => {
        val oauthService = getOauthService
        val accessToken = new Token(k,accSecret.get)

        var totalChange = 1.0

        //get the data
        val profileData = getProfileData(oauthService,accessToken).getBody()
        val connectionData = getConnectionData(oauthService,accessToken).getBody()
        val gson = new Gson() //created to make the objects
        val connections = gson.fromJson(connectionData,classOf[Connections])

        //sort
        import scala.collection.JavaConversions._
        connections.values = connections.values.sortWith((x,y) => x.firstName < y.firstName)
        val myProfile = gson.fromJson(profileData,classOf[Profile])
        connections.values = (myProfile :: connections.values.toList).toArray



       /* connections.values.map{ friend =>
          // connection name
          val name = friend.firstName + " " + friend.lastName
          // List (company name, ticker, industry)
          val theirStocks = getPositions(friend.positions)
          // Lsit (ticker)
          val tickers = theirStocks.map(_._2)
          // List (company name, ticker, price, mrktCap, p/e, div)
          val stockInfo = tickers.map(getStockData(_))
          stockInfo
        }*/
        // Map(ticker, (company name, industry, price, mrktCap, p/e, div, Connections)
        //val map = List.empty[(String, String, String, Double, Double, Double, Double, List[String])]
        val map = getStockInfoByTicker(connections.values)
        Ok(views.html.index.render(myProfile, map))
      }
      case _ =>{
        Logger.info("Redirecting to auth page")
        Redirect(routes.Application.auth())
      }
    }
  }


  def getStockInfoByTicker(values: Array[json.Profile]): List[(String, String, String, Double, Double, Double, Double, List[String])]={
    var peoples = scala.collection.mutable.Map[String,List[(String,String,String)]]()
    values.toList.foldLeft(peoples)((p,v) => {
      val name = v.firstName+" "+v.lastName;
      val positions = getPositions(v.positions)
      p+=((name,positions))})
    val CompanyToPeople = getPossibleStocks(peoples.map(i=> (i._1,i._2.map(j=>(j._1,j._3)))).toMap)
    val CompanyToTicker = peoples.flatMap(i=>i._2.map(j=>(j._1,j._2))).toSet.toMap
    val StockInfo = CompanyToTicker.map(i=>(i._1,getStockData(i._2))).toMap
    var StockList = scala.collection.mutable.MutableList[(String, String, String, Double, Double, Double, Double, List[String])]()
    StockInfo.foldLeft(StockList)((s,i)=>s+=((CompanyToTicker(i._1).toUpperCase,i._1,CompanyToPeople(i._1)._1,StockInfo(i._1)._3,StockInfo(i._1)._4,StockInfo(i._1)._5,StockInfo(i._1)._6,CompanyToPeople(i._1)._2)))
    //StockList.foreach(i=>println("Mymap:"+i._1+" "+i._2+" "+i._3+" "+i._4+" "+i._5+" "+i._6+" "+i._7))
    StockList.toList
  }

  // invert map of companies people have worked for to map of people who have worked for companies
  def getPossibleStocks(peoples : Map[String, List[(String, String)]]):Map[String, (String, List[String])] = {
    //val companies = peoples.flatmap{i => i._2.map{j => j._1}}.toSet
    var CompanyToPeople = scala.collection.mutable.Map[(String,String),List[String]]()
    peoples.map{i=> i._2.foreach{j=>if(!(CompanyToPeople contains j)) CompanyToPeople(j)=List[String](); CompanyToPeople(j)=CompanyToPeople(j)++List(i._1);}}
    var companies = scala.collection.mutable.Map[String,(String,List[String])]()
    CompanyToPeople.map{i=>companies+=((i._1._1,(i._1._2,i._2)))}
    companies.toMap
  }

  def getPositions(positions: Any):List[(String, String, String)] = {
    import scala.collection.JavaConversions._
    if (positions==null||(!positions.asInstanceOf[java.util.LinkedHashMap[String, Any]].containsKey("values")))
      List.empty[(String, String, String)]
    else {
      var myPositions = positions.asInstanceOf[java.util.LinkedHashMap[String, Any]].get("values").asInstanceOf[java.util.ArrayList[java.util.HashMap[String, java.util.HashMap[String, Any]]]].toList
      myPositions = myPositions.filter(_.get("company").containsKey("name"))
      myPositions = myPositions.filter(_.get("company").containsKey("ticker"))
      myPositions = myPositions.filter(_.get("company").containsKey("industry"))
      println("my posistions length %d".format(myPositions.size))

      val stocks = myPositions.map{ p =>
        val ticker = if (p.get("company").containsKey("ticker")) p.get("company").get("ticker").toString else ""
        val companyName = p.get("company").get("name").toString
        println(p.get("company").get("industry"))
        val industry = p.get("company").get("industry").toString
        (companyName, ticker, industry)
      }
      stocks
    }
  }

  //the auth action step
  def auth = Action {
    val serv = getOauthService
    val reqT = serv.getRequestToken()
    val authURL = serv.getAuthorizationUrl(reqT)
    //redirect and store request in cookie
    Redirect(authURL).withSession(
        "req_token" -> reqT.getToken(),
        "req_secret" -> reqT.getSecret()
    )
  }

  //the callback route for oauth
  def callback(oauth_token: String, oauth_verifier: String) = Action { implicit request =>
    val reqToken = session.get("req_token")
    val reqSecret = session.get("req_secret")
    reqToken match {
      case Some(k:String) if k==oauth_token => {
        Logger.error("Tokens DID match")
        val serv = getOauthService
        val verifier = new Verifier(oauth_verifier)
        val reqT = new Token(k, request.session.get("req_secret").get)
        val accessToken = serv.getAccessToken(reqT,verifier)
        Redirect(routes.Application.index()).withSession(
              "acc_token" -> accessToken.getToken(),
              "acc_secret" -> accessToken.getSecret()
              )
      }
      case _ => {
        Logger.error("Tokens didn't match")
        Redirect(routes.Application.auth())
      }
    }
  }

  //the following functions should be in some helper class
  def getOauthService:OAuthService = {
    //could extract this to be checked on startup
    val key = Play.current.configuration.getString("linkedin.apiKey") match {
      case Some(k:String) => k
      case _ => throw new Exception("no linkedin api key set in configuration")
    }
    val secret = Play.current.configuration.getString("linkedin.apiSecret") match {
      case Some(k:String) => k
      case _ => throw new Exception("no linkedin api secret set in configuration")
    }
    val callback = Play.current.configuration.getString("linkedin.callback") match {
      case Some(k:String) => k
      case _ => throw new Exception("no linkedin api callback url set in configuration")
    }
    //get the oauth service
    val sb:ServiceBuilder = new ServiceBuilder()
        .provider(classOf[LinkedInApi])
        .apiKey(key)
        .apiSecret(secret)
        .callback(callback)
    sb.build
  }

  def getProfileData(oauthService:OAuthService,accessToken:Token):Response = {
    val fields = "(id,first-name,last-name,summary,industry,headline,picture-url,positions:(company:(name,ticker,industry),start-date,end-date))"
    val requestURL = "http://api.linkedin.com/v1/people/~:"+fields+"?format=json"
    val req = new OAuthRequest(Verb.GET, requestURL);
    val oauthService = getOauthService
    oauthService.signRequest(accessToken, req);
    req.send();
  }

  def getConnectionData(oauthService:OAuthService,accessToken:Token):Response = {
    val fields = "(id,first-name,last-name,summary,industry,headline,picture-url,positions:(company:(name,ticker,industry),start-date,end-date))"
    val requestURL = "http://api.linkedin.com/v1/people/~/connections:"+fields+"?format=json"
    val req = new OAuthRequest(Verb.GET, requestURL);
    oauthService.signRequest(accessToken, req);
    req.send();
  }


  // ticker -> Name, Symbol, Price, Market Cap, P/E, Div
  def getStockData(ticker: String) : (String, String, Double, Double, Double, Double) = {
    try {
      import scala.collection.JavaConversions._
      val url = "http://finance.yahoo.com/d/quotes.csv?s=%s&f=soj1rdn".format(ticker)
      val connection = new URL(url).openConnection
      val lines = Source.fromInputStream(connection.getInputStream).getLines.next
      val fields = lines.split(",")
      val mrktCap = (if (fields(2).endsWith("B")) 1000 else 1) * fields(2).substring(0, fields(2).length-1).toDouble
      (fields(5).mkString, fields(0).mkString, fields(1).toDouble, mrktCap, fields(3).toDouble, fields(4).toDouble)
    } catch {
      case e: Exception => ("", "", 0.0, 0.0, 0.0, 0.0)
    }
  }
}
