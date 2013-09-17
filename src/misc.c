/* Copyright (C) 2012, 2013 Free Software Foundation, Inc.
 *
 * This file is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

#include "definitions.h"

SCM_DEFINE(scm_irc_get_version, "irc-get-version", 0, 0, 0,
           (), "Get libircclient version")
{
  unsigned int low;
  unsigned int high;
  irc_get_version(&high, &low);
  return scm_cons(scm_from_unsigned_integer(high),
                  scm_from_unsigned_integer(low));
}

SCM_DEFINE(scm_irc_target_get_nick, "irc-target-get-nick", 1, 0, 0,
           (SCM scm_target),  "Gets nick part of target.")
{
  char buf[1024];
  char *target = guarded_scm_to_ascii_string(scm_target);
  irc_target_get_nick(target, buf, sizeof(buf));
  return scm_from_locale_string(buf);
}
