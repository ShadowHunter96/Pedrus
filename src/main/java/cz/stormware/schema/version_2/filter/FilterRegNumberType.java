//
// This file was generated by the Eclipse Implementation of JAXB, v3.0.2 
// See https://eclipse-ee4j.github.io/jaxb-ri 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2022.09.09 at 03:15:47 PM CEST 
//


package cz.stormware.schema.version_2.filter;

import java.math.BigInteger;
import javax.xml.datatype.XMLGregorianCalendar;
import cz.stormware.schema.version_2.type.ExtIdType;
import cz.stormware.schema.version_2.type.StockItemRegNumberType;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlType;


/**
 * <p>Java class for filterRegNumberType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="filterRegNumberType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;all&gt;
 *         &lt;element name="id" type="{http://www.stormware.cz/schema/version_2/type.xsd}idType" minOccurs="0"/&gt;
 *         &lt;element name="extId" type="{http://www.stormware.cz/schema/version_2/type.xsd}extIdType" minOccurs="0"/&gt;
 *         &lt;element name="regNumber" type="{http://www.stormware.cz/schema/version_2/type.xsd}string48" minOccurs="0"/&gt;
 *         &lt;element name="stockItem" type="{http://www.stormware.cz/schema/version_2/type.xsd}stockItemRegNumberType" minOccurs="0"/&gt;
 *         &lt;element name="lastChanges" type="{http://www.w3.org/2001/XMLSchema}dateTime" minOccurs="0"/&gt;
 *       &lt;/all&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "filterRegNumberType", propOrder = {

})
public class FilterRegNumberType {

    protected BigInteger id;
    protected ExtIdType extId;
    protected String regNumber;
    protected StockItemRegNumberType stockItem;
    @XmlSchemaType(name = "dateTime")
    protected XMLGregorianCalendar lastChanges;

    /**
     * Gets the value of the id property.
     * 
     * @return
     *     possible object is
     *     {@link BigInteger }
     *     
     */
    public BigInteger getId() {
        return id;
    }

    /**
     * Sets the value of the id property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigInteger }
     *     
     */
    public void setId(BigInteger value) {
        this.id = value;
    }

    /**
     * Gets the value of the extId property.
     * 
     * @return
     *     possible object is
     *     {@link ExtIdType }
     *     
     */
    public ExtIdType getExtId() {
        return extId;
    }

    /**
     * Sets the value of the extId property.
     * 
     * @param value
     *     allowed object is
     *     {@link ExtIdType }
     *     
     */
    public void setExtId(ExtIdType value) {
        this.extId = value;
    }

    /**
     * Gets the value of the regNumber property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getRegNumber() {
        return regNumber;
    }

    /**
     * Sets the value of the regNumber property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setRegNumber(String value) {
        this.regNumber = value;
    }

    /**
     * Gets the value of the stockItem property.
     * 
     * @return
     *     possible object is
     *     {@link StockItemRegNumberType }
     *     
     */
    public StockItemRegNumberType getStockItem() {
        return stockItem;
    }

    /**
     * Sets the value of the stockItem property.
     * 
     * @param value
     *     allowed object is
     *     {@link StockItemRegNumberType }
     *     
     */
    public void setStockItem(StockItemRegNumberType value) {
        this.stockItem = value;
    }

    /**
     * Gets the value of the lastChanges property.
     * 
     * @return
     *     possible object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public XMLGregorianCalendar getLastChanges() {
        return lastChanges;
    }

    /**
     * Sets the value of the lastChanges property.
     * 
     * @param value
     *     allowed object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public void setLastChanges(XMLGregorianCalendar value) {
        this.lastChanges = value;
    }

}
