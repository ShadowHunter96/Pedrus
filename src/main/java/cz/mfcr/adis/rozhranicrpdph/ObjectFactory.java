
package cz.mfcr.adis.rozhranicrpdph;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the cz.mfcr.adis.rozhranicrpdph package. 
 * <p>An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _StatusNespolehlivyPlatceRequest_QNAME = new QName("http://adis.mfcr.cz/rozhraniCRPDPH/", "StatusNespolehlivyPlatceRequest");
    private final static QName _StatusNespolehlivyPlatceRozsirenyRequest_QNAME = new QName("http://adis.mfcr.cz/rozhraniCRPDPH/", "StatusNespolehlivyPlatceRozsirenyRequest");
    private final static QName _StatusNespolehlivySubjektRozsirenyRequest_QNAME = new QName("http://adis.mfcr.cz/rozhraniCRPDPH/", "StatusNespolehlivySubjektRozsirenyRequest");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: cz.mfcr.adis.rozhranicrpdph
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link StatusNespolehlivyPlatceRequest }
     * 
     */
    public StatusNespolehlivyPlatceRequest createStatusNespolehlivyPlatceRequest() {
        return new StatusNespolehlivyPlatceRequest();
    }

    /**
     * Create an instance of {@link StatusNespolehlivyPlatceResponse }
     * 
     */
    public StatusNespolehlivyPlatceResponse createStatusNespolehlivyPlatceResponse() {
        return new StatusNespolehlivyPlatceResponse();
    }

    /**
     * Create an instance of {@link StatusType }
     * 
     */
    public StatusType createStatusType() {
        return new StatusType();
    }

    /**
     * Create an instance of {@link InformaceOPlatciType }
     * 
     */
    public InformaceOPlatciType createInformaceOPlatciType() {
        return new InformaceOPlatciType();
    }

    /**
     * Create an instance of {@link StatusNespolehlivyPlatceRozsirenyResponse }
     * 
     */
    public StatusNespolehlivyPlatceRozsirenyResponse createStatusNespolehlivyPlatceRozsirenyResponse() {
        return new StatusNespolehlivyPlatceRozsirenyResponse();
    }

    /**
     * Create an instance of {@link InformaceOPlatciRozsireneType }
     * 
     */
    public InformaceOPlatciRozsireneType createInformaceOPlatciRozsireneType() {
        return new InformaceOPlatciRozsireneType();
    }

    /**
     * Create an instance of {@link StatusNespolehlivySubjektRozsirenyResponse }
     * 
     */
    public StatusNespolehlivySubjektRozsirenyResponse createStatusNespolehlivySubjektRozsirenyResponse() {
        return new StatusNespolehlivySubjektRozsirenyResponse();
    }

    /**
     * Create an instance of {@link InformaceOSubjektuRozsireneType }
     * 
     */
    public InformaceOSubjektuRozsireneType createInformaceOSubjektuRozsireneType() {
        return new InformaceOSubjektuRozsireneType();
    }

    /**
     * Create an instance of {@link SeznamNespolehlivyPlatceResponse }
     * 
     */
    public SeznamNespolehlivyPlatceResponse createSeznamNespolehlivyPlatceResponse() {
        return new SeznamNespolehlivyPlatceResponse();
    }

    /**
     * Create an instance of {@link SeznamNespolehlivyPlatceRequest }
     * 
     */
    public SeznamNespolehlivyPlatceRequest createSeznamNespolehlivyPlatceRequest() {
        return new SeznamNespolehlivyPlatceRequest();
    }

    /**
     * Create an instance of {@link Adresa }
     * 
     */
    public Adresa createAdresa() {
        return new Adresa();
    }

    /**
     * Create an instance of {@link StandardniUcetType }
     * 
     */
    public StandardniUcetType createStandardniUcetType() {
        return new StandardniUcetType();
    }

    /**
     * Create an instance of {@link ZverejnenyUcetType }
     * 
     */
    public ZverejnenyUcetType createZverejnenyUcetType() {
        return new ZverejnenyUcetType();
    }

    /**
     * Create an instance of {@link SeznamZverejnenychUctuType }
     * 
     */
    public SeznamZverejnenychUctuType createSeznamZverejnenychUctuType() {
        return new SeznamZverejnenychUctuType();
    }

    /**
     * Create an instance of {@link NestandardniUcetType }
     * 
     */
    public NestandardniUcetType createNestandardniUcetType() {
        return new NestandardniUcetType();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link StatusNespolehlivyPlatceRequest }{@code >}
     * 
     * @param value
     *     Java instance representing xml element's value.
     * @return
     *     the new instance of {@link JAXBElement }{@code <}{@link StatusNespolehlivyPlatceRequest }{@code >}
     */
    @XmlElementDecl(namespace = "http://adis.mfcr.cz/rozhraniCRPDPH/", name = "StatusNespolehlivyPlatceRequest")
    public JAXBElement<StatusNespolehlivyPlatceRequest> createStatusNespolehlivyPlatceRequest(StatusNespolehlivyPlatceRequest value) {
        return new JAXBElement<StatusNespolehlivyPlatceRequest>(_StatusNespolehlivyPlatceRequest_QNAME, StatusNespolehlivyPlatceRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link StatusNespolehlivyPlatceRequest }{@code >}
     * 
     * @param value
     *     Java instance representing xml element's value.
     * @return
     *     the new instance of {@link JAXBElement }{@code <}{@link StatusNespolehlivyPlatceRequest }{@code >}
     */
    @XmlElementDecl(namespace = "http://adis.mfcr.cz/rozhraniCRPDPH/", name = "StatusNespolehlivyPlatceRozsirenyRequest")
    public JAXBElement<StatusNespolehlivyPlatceRequest> createStatusNespolehlivyPlatceRozsirenyRequest(StatusNespolehlivyPlatceRequest value) {
        return new JAXBElement<StatusNespolehlivyPlatceRequest>(_StatusNespolehlivyPlatceRozsirenyRequest_QNAME, StatusNespolehlivyPlatceRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link StatusNespolehlivyPlatceRequest }{@code >}
     * 
     * @param value
     *     Java instance representing xml element's value.
     * @return
     *     the new instance of {@link JAXBElement }{@code <}{@link StatusNespolehlivyPlatceRequest }{@code >}
     */
    @XmlElementDecl(namespace = "http://adis.mfcr.cz/rozhraniCRPDPH/", name = "StatusNespolehlivySubjektRozsirenyRequest")
    public JAXBElement<StatusNespolehlivyPlatceRequest> createStatusNespolehlivySubjektRozsirenyRequest(StatusNespolehlivyPlatceRequest value) {
        return new JAXBElement<StatusNespolehlivyPlatceRequest>(_StatusNespolehlivySubjektRozsirenyRequest_QNAME, StatusNespolehlivyPlatceRequest.class, null, value);
    }

}
