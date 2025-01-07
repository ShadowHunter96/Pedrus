//
// This file was generated by the Eclipse Implementation of JAXB, v3.0.2 
// See https://eclipse-ee4j.github.io/jaxb-ri 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2022.09.09 at 03:15:47 PM CEST 
//


package cz.stormware.schema.version_2.type;

import java.math.BigInteger;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlType;


/**
 * <p>Java class for orderStockItemType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="orderStockItemType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;all&gt;
 *         &lt;element name="actionType" type="{http://www.stormware.cz/schema/version_2/type.xsd}actionTypeStockItem" minOccurs="0"/&gt;
 *         &lt;element name="stockOrder" type="{http://www.w3.org/2001/XMLSchema}integer" minOccurs="0"/&gt;
 *         &lt;element name="stockItem" type="{http://www.stormware.cz/schema/version_2/type.xsd}stockRefType" minOccurs="0"/&gt;
 *       &lt;/all&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "orderStockItemType", propOrder = {

})
public class OrderStockItemType {

    protected ActionTypeStockItem actionType;
    protected BigInteger stockOrder;
    protected StockRefType stockItem;

    /**
     * Gets the value of the actionType property.
     * 
     * @return
     *     possible object is
     *     {@link ActionTypeStockItem }
     *     
     */
    public ActionTypeStockItem getActionType() {
        return actionType;
    }

    /**
     * Sets the value of the actionType property.
     * 
     * @param value
     *     allowed object is
     *     {@link ActionTypeStockItem }
     *     
     */
    public void setActionType(ActionTypeStockItem value) {
        this.actionType = value;
    }

    /**
     * Gets the value of the stockOrder property.
     * 
     * @return
     *     possible object is
     *     {@link BigInteger }
     *     
     */
    public BigInteger getStockOrder() {
        return stockOrder;
    }

    /**
     * Sets the value of the stockOrder property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigInteger }
     *     
     */
    public void setStockOrder(BigInteger value) {
        this.stockOrder = value;
    }

    /**
     * Gets the value of the stockItem property.
     * 
     * @return
     *     possible object is
     *     {@link StockRefType }
     *     
     */
    public StockRefType getStockItem() {
        return stockItem;
    }

    /**
     * Sets the value of the stockItem property.
     * 
     * @param value
     *     allowed object is
     *     {@link StockRefType }
     *     
     */
    public void setStockItem(StockRefType value) {
        this.stockItem = value;
    }

}
