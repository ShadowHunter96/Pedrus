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
 * <p>Java class for sourceDocumentItemType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="sourceDocumentItemType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;all&gt;
 *         &lt;element name="sourceItemId" type="{http://www.stormware.cz/schema/version_2/type.xsd}idType"/&gt;
 *         &lt;element name="sourceItemExtId" type="{http://www.stormware.cz/schema/version_2/type.xsd}extIdType" minOccurs="0"/&gt;
 *       &lt;/all&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "sourceDocumentItemType", propOrder = {

})
public class SourceDocumentItemType {

    @XmlElement(required = true)
    protected BigInteger sourceItemId;
    protected ExtIdType sourceItemExtId;

    /**
     * Gets the value of the sourceItemId property.
     * 
     * @return
     *     possible object is
     *     {@link BigInteger }
     *     
     */
    public BigInteger getSourceItemId() {
        return sourceItemId;
    }

    /**
     * Sets the value of the sourceItemId property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigInteger }
     *     
     */
    public void setSourceItemId(BigInteger value) {
        this.sourceItemId = value;
    }

    /**
     * Gets the value of the sourceItemExtId property.
     * 
     * @return
     *     possible object is
     *     {@link ExtIdType }
     *     
     */
    public ExtIdType getSourceItemExtId() {
        return sourceItemExtId;
    }

    /**
     * Sets the value of the sourceItemExtId property.
     * 
     * @param value
     *     allowed object is
     *     {@link ExtIdType }
     *     
     */
    public void setSourceItemExtId(ExtIdType value) {
        this.sourceItemExtId = value;
    }

}
