//
// This file was generated by the Eclipse Implementation of JAXB, v3.0.2 
// See https://eclipse-ee4j.github.io/jaxb-ri 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2022.09.09 at 03:15:47 PM CEST 
//


package cz.stormware.schema.version_2.type;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlType;


/**
 * Adresa účetní jednotky.
 * 
 * <p>Java class for addressInternetType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="addressInternetType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;all&gt;
 *         &lt;element name="company" type="{http://www.stormware.cz/schema/version_2/type.xsd}stringCompany" minOccurs="0"/&gt;
 *         &lt;element name="title" type="{http://www.stormware.cz/schema/version_2/type.xsd}string7" minOccurs="0"/&gt;
 *         &lt;element name="surname" type="{http://www.stormware.cz/schema/version_2/type.xsd}string32" minOccurs="0"/&gt;
 *         &lt;element name="name" type="{http://www.stormware.cz/schema/version_2/type.xsd}string32" minOccurs="0"/&gt;
 *         &lt;element name="city" type="{http://www.stormware.cz/schema/version_2/type.xsd}string45" minOccurs="0"/&gt;
 *         &lt;element name="street" type="{http://www.stormware.cz/schema/version_2/type.xsd}string64" minOccurs="0"/&gt;
 *         &lt;element name="number" type="{http://www.stormware.cz/schema/version_2/type.xsd}string10" minOccurs="0"/&gt;
 *         &lt;element name="zip" type="{http://www.stormware.cz/schema/version_2/type.xsd}string15" minOccurs="0"/&gt;
 *         &lt;element name="ico" type="{http://www.stormware.cz/schema/version_2/type.xsd}icoType" minOccurs="0"/&gt;
 *         &lt;element name="dic" type="{http://www.stormware.cz/schema/version_2/type.xsd}dicType" minOccurs="0"/&gt;
 *         &lt;element name="icDph" type="{http://www.stormware.cz/schema/version_2/type.xsd}icDphType" minOccurs="0"/&gt;
 *         &lt;element name="phone" type="{http://www.stormware.cz/schema/version_2/type.xsd}string40" minOccurs="0"/&gt;
 *         &lt;element name="mobilPhone" type="{http://www.stormware.cz/schema/version_2/type.xsd}string24" minOccurs="0"/&gt;
 *         &lt;element name="fax" type="{http://www.stormware.cz/schema/version_2/type.xsd}string24" minOccurs="0"/&gt;
 *         &lt;element name="email" type="{http://www.stormware.cz/schema/version_2/type.xsd}string98" minOccurs="0"/&gt;
 *         &lt;element name="www" type="{http://www.stormware.cz/schema/version_2/type.xsd}string96" minOccurs="0"/&gt;
 *       &lt;/all&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "addressInternetType", propOrder = {

})
public class AddressInternetType {

    protected String company;
    protected String title;
    protected String surname;
    protected String name;
    protected String city;
    protected String street;
    protected String number;
    protected String zip;
    protected String ico;
    protected String dic;
    protected String icDph;
    protected String phone;
    protected String mobilPhone;
    protected String fax;
    protected String email;
    protected String www;

    /**
     * Gets the value of the company property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCompany() {
        return company;
    }

    /**
     * Sets the value of the company property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCompany(String value) {
        this.company = value;
    }

    /**
     * Gets the value of the title property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTitle() {
        return title;
    }

    /**
     * Sets the value of the title property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTitle(String value) {
        this.title = value;
    }

    /**
     * Gets the value of the surname property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSurname() {
        return surname;
    }

    /**
     * Sets the value of the surname property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSurname(String value) {
        this.surname = value;
    }

    /**
     * Gets the value of the name property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the value of the name property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setName(String value) {
        this.name = value;
    }

    /**
     * Gets the value of the city property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCity() {
        return city;
    }

    /**
     * Sets the value of the city property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCity(String value) {
        this.city = value;
    }

    /**
     * Gets the value of the street property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getStreet() {
        return street;
    }

    /**
     * Sets the value of the street property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setStreet(String value) {
        this.street = value;
    }

    /**
     * Gets the value of the number property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getNumber() {
        return number;
    }

    /**
     * Sets the value of the number property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setNumber(String value) {
        this.number = value;
    }

    /**
     * Gets the value of the zip property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getZip() {
        return zip;
    }

    /**
     * Sets the value of the zip property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setZip(String value) {
        this.zip = value;
    }

    /**
     * Gets the value of the ico property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getIco() {
        return ico;
    }

    /**
     * Sets the value of the ico property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setIco(String value) {
        this.ico = value;
    }

    /**
     * Gets the value of the dic property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDic() {
        return dic;
    }

    /**
     * Sets the value of the dic property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDic(String value) {
        this.dic = value;
    }

    /**
     * Gets the value of the icDph property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getIcDph() {
        return icDph;
    }

    /**
     * Sets the value of the icDph property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setIcDph(String value) {
        this.icDph = value;
    }

    /**
     * Gets the value of the phone property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPhone() {
        return phone;
    }

    /**
     * Sets the value of the phone property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPhone(String value) {
        this.phone = value;
    }

    /**
     * Gets the value of the mobilPhone property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getMobilPhone() {
        return mobilPhone;
    }

    /**
     * Sets the value of the mobilPhone property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setMobilPhone(String value) {
        this.mobilPhone = value;
    }

    /**
     * Gets the value of the fax property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFax() {
        return fax;
    }

    /**
     * Sets the value of the fax property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFax(String value) {
        this.fax = value;
    }

    /**
     * Gets the value of the email property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getEmail() {
        return email;
    }

    /**
     * Sets the value of the email property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setEmail(String value) {
        this.email = value;
    }

    /**
     * Gets the value of the www property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getWww() {
        return www;
    }

    /**
     * Sets the value of the www property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setWww(String value) {
        this.www = value;
    }

}
