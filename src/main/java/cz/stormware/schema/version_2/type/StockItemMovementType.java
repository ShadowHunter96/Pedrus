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
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;


/**
 * Údaje skladové zásoby pohybů.
 * 
 * <p>Java class for stockItemMovementType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="stockItemMovementType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;all&gt;
 *         &lt;element name="storage" type="{http://www.stormware.cz/schema/version_2/type.xsd}refTypeStorage" minOccurs="0"/&gt;
 *         &lt;element name="stockItem" minOccurs="0"&gt;
 *           &lt;complexType&gt;
 *             &lt;complexContent&gt;
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *                 &lt;all&gt;
 *                   &lt;element name="id" type="{http://www.stormware.cz/schema/version_2/type.xsd}idType" minOccurs="0"/&gt;
 *                   &lt;element name="EAN" type="{http://www.stormware.cz/schema/version_2/type.xsd}string20" minOccurs="0"/&gt;
 *                   &lt;element name="ids" type="{http://www.stormware.cz/schema/version_2/type.xsd}stockIdsType" minOccurs="0"/&gt;
 *                   &lt;element name="name" type="{http://www.stormware.cz/schema/version_2/type.xsd}string90" minOccurs="0"/&gt;
 *                   &lt;element name="nameComplement" type="{http://www.stormware.cz/schema/version_2/type.xsd}string90" minOccurs="0"/&gt;
 *                 &lt;/all&gt;
 *               &lt;/restriction&gt;
 *             &lt;/complexContent&gt;
 *           &lt;/complexType&gt;
 *         &lt;/element&gt;
 *       &lt;/all&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "stockItemMovementType", propOrder = {

})
public class StockItemMovementType {

    protected RefTypeStorage storage;
    protected StockItemMovementType.StockItem stockItem;

    /**
     * Gets the value of the storage property.
     * 
     * @return
     *     possible object is
     *     {@link RefTypeStorage }
     *     
     */
    public RefTypeStorage getStorage() {
        return storage;
    }

    /**
     * Sets the value of the storage property.
     * 
     * @param value
     *     allowed object is
     *     {@link RefTypeStorage }
     *     
     */
    public void setStorage(RefTypeStorage value) {
        this.storage = value;
    }

    /**
     * Gets the value of the stockItem property.
     * 
     * @return
     *     possible object is
     *     {@link StockItemMovementType.StockItem }
     *     
     */
    public StockItemMovementType.StockItem getStockItem() {
        return stockItem;
    }

    /**
     * Sets the value of the stockItem property.
     * 
     * @param value
     *     allowed object is
     *     {@link StockItemMovementType.StockItem }
     *     
     */
    public void setStockItem(StockItemMovementType.StockItem value) {
        this.stockItem = value;
    }


    /**
     * <p>Java class for anonymous complex type.
     * 
     * <p>The following schema fragment specifies the expected content contained within this class.
     * 
     * <pre>
     * &lt;complexType&gt;
     *   &lt;complexContent&gt;
     *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
     *       &lt;all&gt;
     *         &lt;element name="id" type="{http://www.stormware.cz/schema/version_2/type.xsd}idType" minOccurs="0"/&gt;
     *         &lt;element name="EAN" type="{http://www.stormware.cz/schema/version_2/type.xsd}string20" minOccurs="0"/&gt;
     *         &lt;element name="ids" type="{http://www.stormware.cz/schema/version_2/type.xsd}stockIdsType" minOccurs="0"/&gt;
     *         &lt;element name="name" type="{http://www.stormware.cz/schema/version_2/type.xsd}string90" minOccurs="0"/&gt;
     *         &lt;element name="nameComplement" type="{http://www.stormware.cz/schema/version_2/type.xsd}string90" minOccurs="0"/&gt;
     *       &lt;/all&gt;
     *     &lt;/restriction&gt;
     *   &lt;/complexContent&gt;
     * &lt;/complexType&gt;
     * </pre>
     * 
     * 
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "", propOrder = {

    })
    public static class StockItem {

        protected BigInteger id;
        @XmlElement(name = "EAN")
        protected String ean;
        protected String ids;
        protected String name;
        protected String nameComplement;

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
         * Gets the value of the ean property.
         * 
         * @return
         *     possible object is
         *     {@link String }
         *     
         */
        public String getEAN() {
            return ean;
        }

        /**
         * Sets the value of the ean property.
         * 
         * @param value
         *     allowed object is
         *     {@link String }
         *     
         */
        public void setEAN(String value) {
            this.ean = value;
        }

        /**
         * Gets the value of the ids property.
         * 
         * @return
         *     possible object is
         *     {@link String }
         *     
         */
        public String getIds() {
            return ids;
        }

        /**
         * Sets the value of the ids property.
         * 
         * @param value
         *     allowed object is
         *     {@link String }
         *     
         */
        public void setIds(String value) {
            this.ids = value;
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
         * Gets the value of the nameComplement property.
         * 
         * @return
         *     possible object is
         *     {@link String }
         *     
         */
        public String getNameComplement() {
            return nameComplement;
        }

        /**
         * Sets the value of the nameComplement property.
         * 
         * @param value
         *     allowed object is
         *     {@link String }
         *     
         */
        public void setNameComplement(String value) {
            this.nameComplement = value;
        }

    }

}
