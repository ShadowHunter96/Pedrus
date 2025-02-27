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
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.XmlValue;


/**
 * <p>Java class for numberType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="numberType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;all&gt;
 *         &lt;element name="id" type="{http://www.stormware.cz/schema/version_2/type.xsd}idType" minOccurs="0"/&gt;
 *         &lt;element name="ids" type="{http://www.stormware.cz/schema/version_2/type.xsd}idsType" minOccurs="0"/&gt;
 *         &lt;element name="numberRequested" minOccurs="0"&gt;
 *           &lt;complexType&gt;
 *             &lt;simpleContent&gt;
 *               &lt;extension base="&lt;http://www.stormware.cz/schema/version_2/type.xsd&gt;string32NotEmpty"&gt;
 *                 &lt;attribute name="checkDuplicity" type="{http://www.stormware.cz/schema/version_2/type.xsd}boolean" default="true" /&gt;
 *               &lt;/extension&gt;
 *             &lt;/simpleContent&gt;
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
@XmlType(name = "numberType", propOrder = {

})
public class NumberType {

    protected BigInteger id;
    protected String ids;
    protected NumberType.NumberRequested numberRequested;

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
     * Gets the value of the numberRequested property.
     * 
     * @return
     *     possible object is
     *     {@link NumberType.NumberRequested }
     *     
     */
    public NumberType.NumberRequested getNumberRequested() {
        return numberRequested;
    }

    /**
     * Sets the value of the numberRequested property.
     * 
     * @param value
     *     allowed object is
     *     {@link NumberType.NumberRequested }
     *     
     */
    public void setNumberRequested(NumberType.NumberRequested value) {
        this.numberRequested = value;
    }


    /**
     * <p>Java class for anonymous complex type.
     * 
     * <p>The following schema fragment specifies the expected content contained within this class.
     * 
     * <pre>
     * &lt;complexType&gt;
     *   &lt;simpleContent&gt;
     *     &lt;extension base="&lt;http://www.stormware.cz/schema/version_2/type.xsd&gt;string32NotEmpty"&gt;
     *       &lt;attribute name="checkDuplicity" type="{http://www.stormware.cz/schema/version_2/type.xsd}boolean" default="true" /&gt;
     *     &lt;/extension&gt;
     *   &lt;/simpleContent&gt;
     * &lt;/complexType&gt;
     * </pre>
     * 
     * 
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "", propOrder = {
        "value"
    })
    public static class NumberRequested {

        @XmlValue
        protected String value;
        @XmlAttribute(name = "checkDuplicity")
        protected Boolean checkDuplicity;

        /**
         * Řetězec o délce 32 znaků.
         * 
         * @return
         *     possible object is
         *     {@link String }
         *     
         */
        public String getValue() {
            return value;
        }

        /**
         * Sets the value of the value property.
         * 
         * @param value
         *     allowed object is
         *     {@link String }
         *     
         */
        public void setValue(String value) {
            this.value = value;
        }

        /**
         * Gets the value of the checkDuplicity property.
         * 
         * @return
         *     possible object is
         *     {@link Boolean }
         *     
         */
        public Boolean getCheckDuplicity() {
            if (checkDuplicity == null) {
                return Boolean.TRUE;
            } else {
                return checkDuplicity;
            }
        }

        /**
         * Sets the value of the checkDuplicity property.
         * 
         * @param value
         *     allowed object is
         *     {@link Boolean }
         *     
         */
        public void setCheckDuplicity(Boolean value) {
            this.checkDuplicity = value;
        }

    }

}
