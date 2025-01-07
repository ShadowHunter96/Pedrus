package cz.bbn.cerberus.commons.xml;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeConstants;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.GregorianCalendar;

public class XmlUtils {

    private XmlUtils() {
    }

    public static XMLGregorianCalendar toXmlGregorianCalendar(LocalDate date) throws DatatypeConfigurationException {
        if (date == null){
            return null;
        }
        GregorianCalendar gcal = GregorianCalendar.from(date.atStartOfDay(ZoneId.systemDefault()));
        XMLGregorianCalendar xmlDate = DatatypeFactory.newInstance().newXMLGregorianCalendar(gcal);
        xmlDate.setTimezone(DatatypeConstants.FIELD_UNDEFINED);
        return xmlDate.normalize();
    }
}
